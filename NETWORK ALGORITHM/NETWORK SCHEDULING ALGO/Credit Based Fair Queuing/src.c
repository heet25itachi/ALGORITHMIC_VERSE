#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

typedef struct {
    int size;
    int weight;
    int flow_id;
} Packet;

typedef struct {
    Packet *items;
    int front, rear, length, capacity;
    double credit;
    double credit_rate;
    int total_bandwidth;
} Queue;

Queue* create_queue(int capacity, double credit_rate, int bandwidth) {
    Queue *q = malloc(sizeof(Queue));
    q->items = malloc(capacity * sizeof(Packet));
    q->front = 0;
    q->rear = -1;
    q->length = 0;
    q->capacity = capacity;
    q->credit = 0.0;
    q->credit_rate = credit_rate;
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

void update_credit(Queue *q, double delta_time) {
    q->credit += q->credit_rate * delta_time;
    if (q->credit < 0) q->credit = 0.0; // Cannot go negative
}

void simulate_cbfq(Packet *packets, int n, int capacity, int bandwidth, double base_rate) {
    Queue *queues[5]; // 5 queues with different weights
    double weights[5] = {1.0, 2.0, 3.0, 4.0, 5.0};
    for (int i = 0; i < 5; i++) {
        queues[i] = create_queue(capacity / 5, weights[i] * base_rate, bandwidth / 5);
    }
    double current_time = 0.0;
    double service_rate = 1000.0; // packets per second
    int dropped = 0;
    printf("=== CBFQ Scheduler ===\n");
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        int queue_idx = packets[i].flow_id % 5;
        update_credit(queues[queue_idx], 0.001); // 1 ms delta time

        if (enqueue(queues[queue_idx], packets[i])) {
            printf("Packet enqueued, size: %d, weight: %d, flow_id: %d, credit: %.2f\n", 
                   packets[i].size, weights[queue_idx], packets[i].flow_id, queues[queue_idx]->credit);
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }

        // Service: find queue with highest credit
        double max_credit = -1.0;
        int max_idx = -1;
        for (int j = 0; j < 5; j++) {
            if (queues[j]->length > 0 && queues[j]->credit > max_credit) {
                max_credit = queues[j]->credit;
                max_idx = j;
            }
        }
        if (max_idx != -1) {
            Packet p = dequeue(queues[max_idx]);
            queues[max_idx]->credit -= service_rate * 0.001; // Spend credit
            printf("Packet dequeued, size: %d, weight: %d, flow_id: %d, credit: %.2f\n", 
                   p.size, weights[max_idx], p.flow_id, queues[max_idx]->credit);
        }
        current_time += 0.001; // 1 ms
    }

    // Drain remaining queues
    for (int queue_idx = 0; queue_idx < 5; queue_idx++) {
        while (queues[queue_idx]->length > 0) {
            Packet p = dequeue(queues[queue_idx]);
            queues[queue_idx]->credit -= service_rate * 0.001;
            printf("Packet dequeued, size: %d, weight: %d, flow_id: %d, credit: %.2f\n", 
                   p.size, weights[queue_idx], p.flow_id, queues[queue_idx]->credit);
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

int main() {
    srand(time(NULL));
    Packet packets[200];
    for (int i = 0; i < 200; i++) {
        packets[i].size = rand() % 100 + 1;
        packets[i].weight = rand() % 5 + 1;
        packets[i].flow_id = rand() % 5;
    }
    printf("=== CBFQ Scheduler ===\n");
    simulate_cbfq(packets, 200, 100, 1000, 1.0);
    return 0;
}
