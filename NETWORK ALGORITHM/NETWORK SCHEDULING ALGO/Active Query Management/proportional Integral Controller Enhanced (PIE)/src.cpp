#include <iostream>
#include <vector>
#include <random>
#include <cmath>

// Define Packet and Queue classes
struct Packet {
    int size;
    double arrival_time;
};

class Queue {
    std::vector<Packet> items;
    int capacity, front, rear, length;
public:
    Queue(int cap) : capacity(cap), front(0), rear(-1), length(0) {
        items.resize(cap);
    }
    bool enqueue(Packet p) {
        if (length >= capacity) return false;
        rear = (rear + 1) % capacity;
        items[rear] = p;
        length++;
        return true;
    }
    Packet dequeue() {
        Packet p = items[front];
        front = (front + 1) % capacity;
        length--;
        return p;
    }
    Packet peek() { return items[front]; }
    int size() { return length; }
};

// Simulate PIE algorithm
void simulate_pie(std::vector<Packet>& packets, double target, double update_interval, double alpha, double beta, double max_drop_prob, double max_burst, int capacity) {
    Queue q(capacity);
    double current_time = 0.0, last_update = 0.0, drop_prob = 0.0, prev_delay = 0.0, burst_time = max_burst;
    int dropped = 0;
    std::mt19937 gen(42);
    std::uniform_real_distribution<> dis(0, 1);
    std::cout << "Initial queue: empty\n";

    for (auto& p : packets) {
        p.arrival_time = current_time;
        double delay = q.size() == 0 ? 0.0 : current_time - q.peek().arrival_time;

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

        bool drop = false;
        if (burst_time < max_burst && delay > target && dis(gen) < drop_prob) drop = true;

        if (drop) {
            printf("Packet dropped, size: %d, queue delay: %.2f, drop prob: %.4f\n", p.size, delay, drop_prob);
            dropped++;
        } else if (q.enqueue(p)) {
            printf("Packet enqueued, size: %d, queue delay: %.2f, drop prob: %.4f\n", p.size, delay, drop_prob);
            Packet deq_p = q.dequeue();
            printf("Packet dequeued, size: %d, queue delay: %.2f\n", deq_p.size, delay);
        } else {
            printf("Queue full, packet dropped, size: %d\n", p.size);
            dropped++;
        }
        current_time += 1.0;
    }

    while (q.size() > 0) {
        double delay = current_time - q.peek().arrival_time;
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
        Packet p = q.dequeue();
        printf("Packet dequeued, size: %d, queue delay: %.2f\n", p.size, delay);
        current_time += 1.0;
    }

    printf("Final queue length: %d\n", q.size());
    printf("Packets dropped: %d\n", dropped);
    printf("Final queue: empty\n");
}

int main() {
    std::mt19937 gen(42);
    std::uniform_int_distribution<> dis(1, 100);
    std::vector<Packet> packets(200);
    for (auto& p : packets) p.size = dis(gen);
    std::cout << "=== PIE ===\n";
    simulate_pie(packets, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100);
    return 0;
}
