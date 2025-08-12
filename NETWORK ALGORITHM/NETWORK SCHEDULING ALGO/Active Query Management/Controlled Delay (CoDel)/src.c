#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

typedef struct {
    int size;
    double arrival_time;
} Packet;

typedef struct {
    Packet *items;
    int front, rear, length, capacity;
} Queue;

Queue* create_queue(int capacity) {
    Queue *q = malloc(sizeof(Queue));
    q->items = malloc(capacity * sizeof(Packet));
    q->front = 0;
    q->rear = -1;
    q->length = 0;
    q->capacity = capacity;
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
    if (q->length == 0) {
        Packet empty = {0, 0};
        return empty;
    }
    Packet p = q->items[q->front];
    q->front = (q->front + 1) % q->capacity;
    q->length--;
    return p;
}

Packet peek(Queue *q) {
    if (q->length == 0) {
        Packet empty = {0, 0};
        return empty;
    }
    return q->items[q->front];
}

void simulate_codel(Packet *packets, int n, double target, double interval, int capacity) {
    Queue *q = create_queue(capacity);
    double current_time = 0.0;
    double first_above_time = 0.0;
    double drop_next = INFINITY;
    int drop_count = 0;
    int dropped = 0;
    printf("Initial queue: empty\n");

    for (int i = 0; i < n; i++) {
        packets[i].arrival_time = current_time;
        if (enqueue(q, packets[i])) {
            printf("Packet enqueued, size: %d\n", packets[i].size);
            while (q->length > 0) {
                Packet p = peek(q);
                double sojourn_time = current_time - p.arrival_time;

                if (sojourn_time < target || q->length <= 4) {
                    first_above_time = 0.0;
                    drop_next = INFINITY;
                    p = dequeue(q);
                    printf("Packet dequeued, size: %d, sojourn time: %.2f\n", p.size, sojourn_time);
                    drop_count = 0;
                } else if (first_above_time == 0.0) {
                    first_above_time = current_time + interval;
                    drop_next = first_above_time;
                    p = dequeue(q);
                    printf("Packet dequeued, size: %d, sojourn time: %.2f\n", p.size, sojourn_time);
                } else if (current_time >= drop_next) {
                    p = dequeue(q);
                    printf("Packet dropped, size: %d, sojourn time: %.2f\n", p.size, sojourn_time);
                    dropped++;
                    drop_count++;
                    drop_next = current_time + interval / sqrt(drop_count);
                } else {
                    p = dequeue(q);
                    printf("Packet dequeued, size: %d, sojourn time: %.2f\n", p.size, sojourn_time);
                    drop_count = 0;
                }
            }
        } else {
            printf("Queue full, packet dropped, size: %d\n", packets[i].size);
            dropped++;
        }
        current_time += 1.0;
    }

    while (q->length > 0) {
        Packet p = peek(q);
        double sojourn_time = current_time - p.arrival_time;

        if (sojourn_time < target || q->length <= 4) {
            first_above_time = 0.0;
            drop_next = INFINITY;
            p = dequeue(q);
            printf("Packet dequeued, size: %d, sojourn time: %.2f\n", p.size, sojourn_time);
            drop_count = 0;
        } else if (first_above_time == 0.0) {
            first_above_time = current_time + interval;
            drop_next = first_above_time;
            p = dequeue(q);
            printf("Packet dequeued, size: %d, sojourn time: %.2f\n", p.size, sojourn_time);
        } else if (current_time >= drop_next) {
            p = dequeue(q);
            printf("Packet dropped, size: %d, sojourn time: %.2f\n", p.size, sojourn_time);
            dropped++;
            drop_count++;
            drop_next = current_time + interval / sqrt(drop_count);
        } else {
            p = dequeue(q);
            printf("Packet dequeued, size: %d, sojourn time: %.2f\n", p.size, sojourn_time);
            drop_count = 0;
        }
        current_time += 1.0;
    }

    printf("Final queue length: %d\n", q->length);
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: empty\n");

    free(q->items);
    free(q);
}

int main() {
    srand(time(NULL));
    int n = 200, capacity = 100;
    double target = 5, interval = 100;
    Packet *packets = malloc(n * sizeof(Packet));
    for (int i = 0; i < n; i++) packets[i].size = rand() % 100 + 1;

    simulate_codel(packets, n, target, interval, capacity);

    free(packets);
    return 0;
}
