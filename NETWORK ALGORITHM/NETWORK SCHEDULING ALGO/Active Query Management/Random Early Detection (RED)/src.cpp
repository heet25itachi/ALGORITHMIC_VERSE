#include <iostream>
#include <vector>
#include <random>

// Define Packet and Queue classes
struct Packet { int size; };
class Queue {
    std::vector<Packet> items;
    int capacity;
public:
    Queue(int cap) : capacity(cap) {}
    bool enqueue(Packet p) {
        if (items.size() >= capacity) return false;
        items.push_back(p);
        return true;
    }
    int size() { return items.size(); }
    void print_queue() {
        std::cout << "Final queue: ";
        for (const auto& p : items) std::cout << p.size << " ";
        std::cout << "\n";
    }
};

// Simulate RED algorithm
void simulate_red(std::vector<Packet>& packets, double min_th, double max_th, double w_q, double max_p, int capacity) {
    Queue q(capacity);
    double avg = 0, count = 0, dropped = 0;
    std::mt19937 gen(42); // Mersenne Twister with seed
    std::uniform_real_distribution<> dis(0, 1);
    std::cout << "Initial queue: empty\n";
    for (const auto& p : packets) {
        avg = q.size() == 0 ? 0 : (1 - w_q) * avg + w_q * q.size();
        bool drop = false;
        if (avg < min_th) {
            drop = false;
        } else if (avg >= max_th) {
            drop = true;
        } else {
            double pb = max_p * (avg - min_th) / (max_th - min_th);
            double pa = pb / (1 - count * pb);
            count++;
            drop = dis(gen) < pa;
        }
        if (drop) {
            printf("Packet dropped, size: %d, avg queue length: %.2f, max_p: %.4f\n", p.size, avg, max_p);
            dropped++;
        } else if (q.enqueue(p)) {
            printf("Packet enqueued, size: %d, avg queue length: %.2f, max_p: %.4f\n", p.size, avg, max_p);
            count = 0;
        } else {
            printf("Queue full, packet dropped, size: %d\n", p.size);
            dropped++;
        }
    }
    printf("Final queue length: %d\n", q.size());
    printf("Packets dropped: %d\n", (int)dropped);
    q.print_queue();
}

int main() {
    std::vector<Packet> packets(200);
    std::mt19937 gen(42);
    std::uniform_int_distribution<> dis(1, 100);
    for (auto& p : packets) p.size = dis(gen);
    std::cout << "=== RED ===\n";
    simulate_red(packets, 20, 80, 0.002, 0.1, 100);
    return 0;
}
