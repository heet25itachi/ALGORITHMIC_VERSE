#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Define Packet and Queue structures
typedef struct { int size; } Packet;
typedef struct { Packet* items; int size; int capacity; } Queue;

// Initialize a new queue
Queue* new_queue(int capacity) {
    Queue* q = malloc(sizeof(Queue));
    q->items = malloc(capacity * sizeof(Packet));
    q->size = 0;
    q->capacity = capacity;
    return q;
}

// Enqueue a packet
int enqueue(Queue* q, Packet p) {
    if (q->size >= q->capacity) return 0;
    q->items[q->size++] = p;
    return 1;
}

// Print queue contents
void print_queue(Queue* q) {
    printf("Final queue: ");
    for (int i = 0; i < q->size; i++) printf("%d ", q->items[i].size);
    printf("\n");
}

// Simulate RED algorithm
void simulate_red(Packet* packets, int n, double min_th, double max_th, double w_q, double max_p, int capacity) {
    Queue* q = new_queue(capacity);
    double avg = 0, count = 0, dropped = 0;
    printf("Initial queue: empty\n");
    for (int i = 0; i < n; i++) {
        avg = q->size == 0 ? 0 : (1 - w_q) * avg + w_q * q->size;
        int drop = 0;
        if (avg < min_th) {
            drop = 0;
        } else if (avg >= max_th) {
            drop = 1;
        } else {
            double pb = max_p * (avg - min_th) / (max_th - min_th);
            double pa = pb / (1 - count * pb);
            count++;
            drop = ((double)rand() / RAND_MAX) < pa;
        }
        if (drop) {
            printf("Packet dropped, size: %d, avg queue length: %.2f, max_p: %.4f\n", packets[i].size, avg, max_p);
            dropped++;
        } else if (enqueue(q, packets[i])) {
            printf("Packet enqueued, size: %d, avg queue length: %.2f, max_p: %.4f\n", packets[i].size, avg, max_p);
            count = 0;
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }
    }
    printf("Final queue length: %d\n", q->size);
    printf("Packets dropped: %d\n", (int)dropped);
    print_queue(q);
    free(q->items); free(q);
}

int main() {
    srand(42); // Set random seed for reproducibility
    Packet packets[200];
    for (int i = 0; i < 200; i++) packets[i].size = rand() % 100 + 1;
    printf("=== RED ===\n");
    simulate_red(packets, 200, 20, 80, 0.002, 0.1, 100);
    return 0;
}
