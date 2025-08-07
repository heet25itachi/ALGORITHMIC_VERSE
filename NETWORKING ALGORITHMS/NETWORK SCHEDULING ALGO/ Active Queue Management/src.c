#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct {
    int size;
} Packet;

typedef struct {
    int *items;
    int front, rear, length, capacity;
} Queue;

Queue* create_queue(int capacity) {
    Queue *q = malloc(sizeof(Queue));
    q->items = malloc(capacity * sizeof(int));
    q->front = 0;
    q->rear = -1;
    q->length = 0;
    q->capacity = capacity;
    return q;
}

int enqueue(Queue *q, int size) {
    if (q->length >= q->capacity) return 0;
    q->rear = (q->rear + 1) % q->capacity;
    q->items[q->rear] = size;
    q->length++;
    return 1;
}

int red_enqueue(Queue *q, Packet p, double min_th, double max_th, double w_q, double max_p, double *avg, int *count) {
    if (q->length == 0) *avg = 0;
    else *avg = (1 - w_q) * (*avg) + w_q * q->length;

    int drop = 0;
    if (*avg < min_th) {
        drop = 0;
    } else if (*avg >= max_th) {
        drop = 1;
    } else {
        double pb = max_p * (*avg - min_th) / (max_th - min_th);
        double pa = pb / (1 - (*count) * pb);
        if ((double)rand() / RAND_MAX < pa) drop = 1;
        else drop = 0;
        (*count)++;
    }

    if (drop) {
        printf("Packet dropped, size: %d, avg queue length: %.2f\n", p.size, *avg);
        return 0;
    } else if (enqueue(q, p.size)) {
        printf("Packet enqueued, size: %d, avg queue length: %.2f\n", p.size, *avg);
        *count = 0;
        return 1;
    } else {
        printf("Queue full, packet dropped, size: %d\n", p.size);
        return 0;
    }
}

void simulate_red(Packet *packets, int n, double min_th, double max_th, double w_q, double max_p, int capacity) {
    Queue *q = create_queue(capacity);
    double avg = 0;
    int count = 0, dropped = 0;
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        if (!red_enqueue(q, packets[i], min_th, max_th, w_q, max_p, &avg, &count)) {
            dropped++;
        }
    }

    printf("Final queue length: %d\n", q->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: ");
    for (int i = 0, idx = q->front; i < q->length; i++, idx = (idx + 1) % q->capacity) {
        printf("%d ", q->items[idx]);
    }
    printf("\n");

    free(q->items);
    free(q);
}

int main() {
    srand(time(NULL));
    int n = 200, capacity = 100;
    double min_th = 20, max_th = 80, w_q = 0.002, max_p = 0.1;
    Packet *packets = malloc(n * sizeof(Packet));
    for (int i = 0; i < n; i++) packets[i].size = rand() % 100 + 1;

    simulate_red(packets, n, min_th, max_th, w_q, max_p, capacity);

    free(packets);
    return 0;
}
