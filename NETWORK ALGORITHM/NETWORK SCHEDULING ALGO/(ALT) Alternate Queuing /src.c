#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

typedef struct {
    int size, priority, flow_id;
} Packet;

typedef struct {
    Packet *items;
    int front, rear, length, capacity;
    int total_bandwidth;
} Queue;

typedef struct {
    Queue **queues;
    int num_queues;
} PRIQ;

typedef struct {
    Queue *queue;
    double bandwidth;
} CBQNode;

typedef struct {
    Queue *queue;
    double target_delay;
    double interval;
    double first_above_time;
    double drop_next;
    int drop_count;
} CoDelQueue;

Queue* create_queue(int capacity, int bandwidth) {
    Queue *q = malloc(sizeof(Queue));
    q->items = malloc(capacity * sizeof(Packet));
    q->front = 0;
    q->rear = -1;
    q->length = 0;
    q->capacity = capacity;
    q->total_bandwidth = bandwidth;
    return q;
}

int enqueue(Queue *q, Packet p) {
    if (q->length >= q->capacity) return 0;
    q->rear = (q->rear + 1) % q->capacity;
    q->items[q->rear] = p;
    q->length++;
    return 1;
}

Packet dequeue(Queue *q) {
    Packet p = q->items[q->front];
    q->front = (q->front + 1) % q->capacity;
    q->length--;
    return p;
}

void simulate_priq(Packet *packets, int n, int capacity, int bandwidth) {
    PRIQ *priq = malloc(sizeof(PRIQ));
    priq->num_queues = 16;
    priq->queues = malloc(16 * sizeof(Queue*));
    for (int i = 0; i < 16; i++) {
        priq->queues[i] = create_queue(capacity / 16, bandwidth / 16);
    }
    int dropped = 0;
    printf("=== PRIQ Scheduler ===\n");
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        int pri = packets[i].priority;
        if (pri < 0 || pri > 15) pri = rand() % 16;
        if (enqueue(priq->queues[pri], packets[i])) {
            printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", packets[i].size, pri, packets[i].flow_id);
        } else {
            printf("Queue full, packet dropped, size: %d, priority: %d\n", packets[i].size, pri);
            dropped++;
        }
    }

    for (int pri = 15; pri >= 0; pri--) {
        while (priq->queues[pri]->length > 0) {
            Packet p = dequeue(priq->queues[pri]);
            printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, pri, p.flow_id);
        }
    }

    printf("Final queue length: 0\n");
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: empty\n");

    for (int i = 0; i < 16; i++) {
        free(priq->queues[i]->items);
        free(priq->queues[i]);
    }
    free(priq->queues);
    free(priq);
}

void simulate_codel(Packet *packets, int n, int capacity, int bandwidth, double target, double interval) {
    CoDelQueue *codel = malloc(sizeof(CoDelQueue));
    codel->queue = create_queue(capacity, bandwidth);
    codel->target_delay = target;
    codel->interval = interval;
    codel->first_above_time = 0.0;
    codel->drop_next = INFINITY;
    codel->drop_count = 0;
    int dropped = 0;
    double current_time = 0.0;
    printf("=== CoDel Scheduler ===\n");
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        if (codel->queue->length == 0) codel->first_above_time = 0.0;
        codel->drop_next = INFINITY;
        codel->drop_count = 0;

        if (enqueue(codel->queue, packets[i])) {
            printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", packets[i].size, packets[i].priority, packets[i].flow_id);
            while (codel->queue->length > 0) {
                Packet front = codel->queue->items[codel->queue->front];
                double sojourn_time = current_time - (double)i; // Simplified sojourn time
                if (sojourn_time < codel->target_delay || codel->queue->length <= 4) {
                    Packet p = dequeue(codel->queue);
                    printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flow_id);
                } else if (codel->first_above_time == 0.0) {
                    codel->first_above_time = current_time + codel->interval;
                    codel->drop_next = codel->first_above_time;
                    Packet p = dequeue(codel->queue);
                    printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flow_id);
                } else if (current_time >= codel->drop_next) {
                    Packet p = dequeue(codel->queue);
                    printf("Packet dropped, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flow_id);
                    dropped++;
                    codel->drop_count++;
                    codel->drop_next = current_time + codel->interval / sqrt(codel->drop_count);
                } else {
                    Packet p = dequeue(codel->queue);
                    printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flow_id);
                    codel->drop_count = 0;
                }
            }
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }
        current_time += 1.0;
    }

    printf("Final queue length: %d\n", codel->queue->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: empty\n");

    free(codel->queue->items);
    free(codel->queue);
    free(codel);
}

void simulate_cbq(Packet *packets, int n, int capacity, int bandwidth) {
    // Simplified CBQ with root queue and child queues
    Queue *root = create_queue(capacity, bandwidth);
    int dropped = 0;
    printf("=== CBQ Scheduler ===\n");
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        if (enqueue(root, packets[i])) {
            printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", packets[i].size, packets[i].priority, packets[i].flow_id);
            // Simulate bandwidth allocation: dequeue if within bandwidth
            if (root->length > capacity / 4) { // Simple bandwidth check
                Packet p = dequeue(root);
                printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flow_id);
            }
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }
    }

    while (root->length > 0) {
        Packet p = dequeue(root);
        printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flow_id);
    }

    printf("Final queue length: %d\n", root->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: empty\n");

    free(root->items);
    free(root);
}

void simulate_fairq(Packet *packets, int n, int capacity, int bandwidth) {
    // Simplified FairQ with round-robin among flows
    Queue *queues[5]; // 5 flows
    for (int i = 0; i < 5; i++) queues[i] = create_queue(capacity / 5, bandwidth / 5);
    int dropped = 0;
    printf("=== FairQ Scheduler ===\n");
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        int flow = packets[i].flow_id % 5;
        if (enqueue(queues[flow], packets[i])) {
            printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", packets[i].size, packets[i].priority, flow);
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }
    }

    for (int flow = 0; flow < 5; flow++) {
        while (queues[flow]->length > 0) {
            Packet p = dequeue(queues[flow]);
            printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, flow);
        }
    }

    printf("Final queue length: 0\n");
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: empty\n");

    for (int i = 0; i < 5; i++) {
        free(queues[i]->items);
        free(queues[i]);
    }
}

void simulate_hfsc(Packet *packets, int n, int capacity, int bandwidth) {
    // Simplified HFSC with hierarchical queues
    Queue *root = create_queue(capacity, bandwidth);
    int dropped = 0;
    printf("=== HFSC Scheduler ===\n");
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        if (enqueue(root, packets[i])) {
            printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", packets[i].size, packets[i].priority, packets[i].flow_id);
            // Simulate service curve: dequeue based on bandwidth
            if (root->length > capacity / 3) { // Simple bandwidth check
                Packet p = dequeue(root);
                printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flow_id);
            }
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }
    }

    while (root->length > 0) {
        Packet p = dequeue(root);
        printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flow_id);
    }

    printf("Final queue length: %d\n", root->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: empty\n");

    free(root->items);
    free(root);
}

int main() {
    srand(time(NULL));
    Packet packets[200];
    for (int i = 0; i < 200; i++) {
        packets[i].size = rand() % 100 + 1;
        packets[i].priority = rand() % 16;
        packets[i].flow_id = rand() % 5;
    }

    simulate_priq(packets, 200, 100, 1000);
    simulate_codel(packets, 200, 100, 1000, 5.0, 100.0);
    simulate_cbq(packets, 200, 100, 1000);
    simulate_fairq(packets, 200, 100, 1000);
    simulate_hfsc(packets, 200, 100, 1000);

    return 0;
}
