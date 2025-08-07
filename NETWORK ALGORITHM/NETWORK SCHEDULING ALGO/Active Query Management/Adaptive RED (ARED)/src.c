#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct { int size; double arrival_time; } Packet;
typedef struct { Packet *items; int front, rear, length, capacity; } Queue;

Queue* create_queue(int capacity) {
    Queue *q = malloc(sizeof(Queue));
    q->items = malloc(capacity * sizeof(Packet));
    q->front = 0; q->rear = -1; q->length = 0; q->capacity = capacity;
    return q;
}

int enqueue(Queue *q, Packet p) {
    if (q->length >= q->capacity) return 0;
    q->rear = (q->rear + 1) % q->capacity;
    q->items[q->rear] = p;
    q->length++;
    return 1;
}

void simulate_ared(Packet *packets, int n, double min_th, double max_th, double w_q, double target, double alpha, double beta, double interval, int capacity) {
    Queue *q = create_queue(capacity);
    double avg = 0, max_p = 0.1, last_update = 0, current_time = 0;
    int count = 0, dropped = 0;
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        if (q->length == 0) avg = 0;
        else avg = (1 - w_q) * avg + w_q * q->length;

        if (current_time - last_update >= interval) {
            if (avg > target && max_p <= 0.5) max_p *= (1 + alpha);
            else if (avg < target && max_p >= 0.01) max_p *= beta;
            last_update = current_time;
        }

        int drop = 0;
        if (avg < min_th) drop = 0;
        else if (avg >= max_th) drop = 1;
        else {
            double pb = max_p * (avg - min_th) / (max_th - min_th);
            double pa = pb / (1 - count * pb);
            drop = (double)rand() / RAND_MAX < pa ? 1 : 0;
            count++;
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
        current_time += 1.0;
    }

    printf("Final queue length: %d\n", q->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: ");
    for (int i = 0, idx = q->front; i < q->length; i++, idx = (idx + 1) % q->capacity)
        printf("%d ", q->items[idx].size);
    printf("\n");
    free(q->items); free(q);
}

void simulate_blue(Packet *packets, int n, double d1, double d2, double freeze_time, int capacity) {
    Queue *q = create_queue(capacity);
    double p = 0, last_update = 0, current_time = 0;
    int dropped = 0;
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        if (q->length >= capacity) {
            p += d1;
            last_update = current_time;
            printf("Queue full, packet dropped, size: %d, drop prob: %.4f\n", packets[i].size, p);
            dropped++;
        } else {
            if (current_time - last_update >= freeze_time && q->length == 0) {
                p = p > d2 ? p - d2 : 0;
                last_update = current_time;
            }
            if ((double)rand() / RAND_MAX < p) {
                printf("Packet dropped, size: %d, drop prob: %.4f\n", packets[i].size, p);
                dropped++;
            } else if (enqueue(q, packets[i])) {
                printf("Packet enqueued, size: %d, drop prob: %.4f\n", packets[i].size, p);
            }
        }
        current_time += 1.0;
    }

    printf("Final queue length: %d\n", q->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: ");
    for (int i = 0, idx = q->front; i < q->length; i++, idx = (idx + 1) % q->capacity)
        printf("%d ", q->items[idx].size);
    printf("\n");
    free(q->items); free(q);
}

void simulate_pi(Packet *packets, int n, double q_ref, double a, double b, int capacity) {
    Queue *q = create_queue(capacity);
    double p = 0, prev_error = 0, current_time = 0;
    int dropped = 0;
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        double error = q->length - q_ref;
        p += a * error - b * prev_error;
        prev_error = error;
        if (p < 0) p = 0; else if (p > 1) p = 1;

        if ((double)rand() / RAND_MAX < p) {
            printf("Packet dropped, size: %d, drop prob: %.4f\n", packets[i].size, p);
            dropped++;
        } else if (enqueue(q, packets[i])) {
            printf("Packet enqueued, size: %d, drop prob: %.4f\n", packets[i].size, p);
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }
        current_time += 1.0;
    }

    printf("Final queue length: %d\n", q->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: ");
    for (int i = 0, idx = q->front; i < q->length; i++, idx = (idx + 1) % q->capacity)
        printf("%d ", q->items[idx].size);
    printf("\n");
    free(q->items); free(q);
}

int main() {
    srand(time(NULL));
    int n = 200, capacity = 100;
    Packet *packets = malloc(n * sizeof(Packet));
    for (int i = 0; i < n; i++) packets[i].size = rand() % 100 + 1;

    printf("=== ARED ===\n");
    simulate_ared(packets, n, 20, 80, 0.002, 50, 0.01, 0.9, 1000, capacity);
    printf("\n=== Blue ===\n");
    simulate_blue(packets, n, 0.0002, 0.00005, 100, capacity);
    printf("\n=== PI ===\n");
    simulate_pi(packets, n, 50, 0.00001822, 0.00001816, capacity);

    free(packets);
    return 0;
}
