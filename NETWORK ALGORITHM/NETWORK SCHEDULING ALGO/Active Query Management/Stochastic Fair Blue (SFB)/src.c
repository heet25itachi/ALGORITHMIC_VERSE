#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct {
    int size;
    int flow_id;
} Packet;

typedef struct {
    Packet *items;
    int front, rear, length, capacity;
} Queue;

typedef struct {
    double p; // drop probability
    double last_update;
} Bin;

typedef struct {
    Bin *bins; // L * N bins
    int L, N;
} SFB;

Queue* create_queue(int capacity) {
    Queue *q = malloc(sizeof(Queue));
    q->items = malloc(capacity * sizeof(Packet));
    q->front = 0;
    q->rear = -1;
    q->length = 0;
    q->capacity = capacity;
    return q;
}

SFB* create_sfb(int L, int N) {
    SFB *sfb = malloc(sizeof(SFB));
    sfb->bins = malloc(L * N * sizeof(Bin));
    sfb->L = L;
    sfb->N = N;
    for (int i = 0; i < L * N; i++) {
        sfb->bins[i].p = 0.0;
        sfb->bins[i].last_update = 0.0;
    }
    return sfb;
}

int hash_flow(int flow_id, int level) {
    return (flow_id + level) % N; // Simple hash
}

int sfb_drop(SFB *sfb, int flow_id, double current_time, double d1, double d2, double freeze_time, int queue_length, int capacity) {
    int marked = 1;
    for (int l = 0; l < sfb->L; l++) {
        int bin_idx = l * sfb->N + hash_flow(flow_id, l);
        Bin *bin = &sfb->bins[bin_idx];
        if (current_time - bin->last_update >= freeze_time) {
            if (queue_length == 0) {
                bin->p = max(bin->p - d2, 0.0);
            } else if (queue_length >= capacity) {
                bin->p += d1;
            }
            bin->last_update = current_time;
        }
        if (bin->p < 1.0) {
            marked = 0;
            break;
        }
    }
    return marked;
}

void simulate_sfb(Packet *packets, int n, double d1, double d2, double freeze_time, int capacity, int L, int N) {
    Queue *q = create_queue(capacity);
    SFB *sfb = create_sfb(L, N);
    double current_time = 0.0;
    int dropped = 0;
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        if (sfb_drop(sfb, packets[i].flow_id, current_time, d1, d2, freeze_time, q->length, capacity)) {
            printf("Packet dropped, size: %d, flow_id: %d\n", packets[i].size, packets[i].flow_id);
            dropped++;
        } else if (enqueue(q, packets[i])) {
            printf("Packet enqueued, size: %d, flow_id: %d\n", packets[i].size, packets[i].flow_id);
            // Immediate dequeue for simulation
            Packet deq_p = dequeue(q);
            printf("Packet dequeued, size: %d, flow_id: %d\n", deq_p.size, deq_p.flow_id);
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }
        current_time += 1.0;
    }

    while (q->length > 0) {
        Packet p = dequeue(q);
        printf("Packet dequeued, size: %d, flow_id: %d\n", p.size, p.flow_id);
        current_time += 1.0;
    }

    printf("Final queue length: %d\n", q->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: empty\n");

    free(q->items); free(q);
    free(sfb->bins); free(sfb);
}

int main() {
    srand(time(NULL));
    int n = 200, capacity = 100, L = 2, N = 4;
    double d1 = 0.0002, d2 = 0.00005, freeze_time = 100;
    Packet packets[200];
    for (int i = 0; i < 200; i++) {
        packets[i].size = rand() % 100 + 1;
        packets[i].flow_id = rand() % 20 + 1;
    }

    printf("=== SFB ===\n");
    simulate_sfb(packets, n, d1, d2, freeze_time, capacity, L, N);

    return 0;
}
