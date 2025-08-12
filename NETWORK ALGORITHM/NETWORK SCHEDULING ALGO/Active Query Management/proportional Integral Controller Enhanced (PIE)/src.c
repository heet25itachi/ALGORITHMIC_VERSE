#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

// Define Packet and Queue structures
typedef struct {
    int size;
    double arrival_time;
} Packet;

typedef struct {
    Packet *items;
    int front, rear, length, capacity;
} Queue;

// Initialize queue
Queue* create_queue(int capacity) {
    Queue *q = malloc(sizeof(Queue));
    q->items = malloc(capacity * sizeof(Packet));
    q->front = 0;
    q->rear = -1;
    q->length = 0;
    q->capacity = capacity;
    return q;
}

// Enqueue packet
int enqueue(Queue *q, Packet p) {
    if (q->length >= q->capacity) return 0;
    q->rear = (q->rear + 1) % q->capacity;
    q->items[q->rear] = p;
    q->length++;
    return 1;
}

// Dequeue packet
Packet dequeue(Queue *q) {
    Packet p = q->items[q->front];
    q->front = (q->front + 1) % q->capacity;
    q->length--;
    return p;
}

// Peek oldest packet
Packet peek(Queue *q) {
    return q->items[q->front];
}

// Simulate PIE algorithm
void simulate_pie(Packet *packets, int n, double target, double update_interval, double alpha, double beta, double max_drop_prob, double max_burst, int capacity) {
    Queue *q = create_queue(capacity);
    double current_time = 0.0, last_update = 0.0, drop_prob = 0.0, prev_delay = 0.0, burst_time = max_burst;
    int dropped = 0;
    printf("Initial queue: empty\n");
    srand(42);

    for (int i = 0; i < n; i++) {
        packets[i].arrival_time = current_time;
        double delay = q->length == 0 ? 0.0 : current_time - peek(q).arrival_time;

        // Update drop probability every update_interval
        if (current_time - last_update >= update_interval) {
            double error = delay - target;
            drop_prob += alpha * error + beta * (delay - prev_delay);
            if (drop_prob < 0) drop_prob = 0;
            if (drop_prob > max_drop_prob) drop_prob = max_drop_prob;
            prev_delay = delay;
            last_update = current_time;
            if (delay > target) burst_time = 0;
            else if (burst_time < max_burst) burst_time += update_interval;
        }

        // Decide whether to drop packet
        int drop = 0;
        if (burst_time >= max_burst || delay <= target) drop = 0;
        else if (((double)rand() / RAND_MAX) < drop_prob) drop = 1;

        if (drop) {
            printf("Packet dropped, size: %d, queue delay: %.2f, drop prob: %.4f\n", packets[i].size, delay, drop_prob);
            dropped++;
        } else if (enqueue(q, packets[i])) {
            printf("Packet enqueued, size: %d, queue delay: %.2f, drop prob: %.4f\n", packets[i].size, delay, drop_prob);
            // Immediate dequeue for simulation
            Packet p = dequeue(q);
            printf("Packet dequeued, size: %d, queue delay: %.2f\n", p.size, delay);
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }
        current_time += 1.0;
    }

    // Process remaining packets
    while (q->length > 0) {
        double delay = current_time - peek(q).arrival_time;
        if (current_time - last_update >= update_interval) {
            double error = delay - target;
            drop_prob += alpha * error + beta * (delay - prev_delay);
            if (drop_prob < 0) drop_prob = 0;
            if (drop_prob > max_drop_prob) drop_prob = max_drop_prob;
            prev_delay = delay;
            last_update = current_time;
            if (delay > target) burst_time = 0;
            else if (burst_time < max_burst) burst_time += update_interval;
        }
        Packet p = dequeue(q);
        printf("Packet dequeued, size: %d, queue delay: %.2f\n", p.size, delay);
        current_time += 1.0;
    }

    printf("Final queue length: %d\n", q->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: empty\n");
    free(q->items);
    free(q);
}

int main() {
    Packet packets[200];
    for (int i = 0; i < 200; i++) packets[i].size = rand() % 100 + 1;
    printf("=== PIE ===\n");
    simulate_pie(packets, 200, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100);
    return 0;
}
