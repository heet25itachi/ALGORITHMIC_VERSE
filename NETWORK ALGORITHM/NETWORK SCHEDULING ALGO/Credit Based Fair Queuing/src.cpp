#include <iostream>
#include <vector>
#include <queue>
#include <random>
#include <cmath>

struct Packet {
    int size, weight, flow_id;
};

class Queue {
    std::queue<Packet> items;
    int capacity;
    double credit, credit_rate;
    int total_bandwidth;
public:
    Queue(int cap, double rate, int bw) : capacity(cap), credit(0.0), credit_rate(rate), total_bandwidth(bw) {}
    bool enqueue(Packet p) {
        if (items.size() >= capacity) return false;
        items.push(p);
        return true;
    }
    Packet dequeue() {
        Packet p = items.front();
        items.pop();
        return p;
    }
    void update_credit(double delta_time) {
        credit += credit_rate * delta_time;
        if (credit < 0) credit = 0.0;
    }
    double get_credit() { return credit; }
    void spend_credit(double amount) { credit -= amount; if (credit < 0) credit = 0.0; }
    int size() { return items.size(); }
};

void simulate_cbfq(std::vector<Packet>& packets, int capacity, int bandwidth, double base_rate) {
    std::vector<Queue*> queues(5);
    double weights[5] = {1.0, 2.0, 3.0, 4.0, 5.0};
    for (int i = 0; i < 5; i++) {
        queues[i] = new Queue(capacity / 5, weights[i] * base_rate, bandwidth / 5);
    }
    double current_time = 0.0;
    double service_rate = 1000.0;
    int dropped = 0;
    std::cout << "=== CBFQ Scheduler ===" << std::endl;
    std::cout << "Initial queue: empty" << std::endl;

    for (auto& p : packets) {
        int queue_idx = p.flow_id % 5;
        queues[queue_idx]->update_credit(0.001);

        if (queues[queue_idx]->enqueue(p)) {
            std::cout << "Packet enqueued, size: " << p.size << ", weight: " << weights[queue_idx] 
                      << ", flow_id: " << p.flow_id << ", credit: " << queues[queue_idx]->get_credit() << std::endl;
        } else {
            std::cout << "Queue full, packet dropped, size: " << p.size << std::endl;
            dropped++;
        }

        // Service: select queue with highest credit
        double max_credit = -1.0;
        int max_idx = -1;
        for (int j = 0; j < 5; j++) {
            if (queues[j]->size() > 0 && queues[j]->get_credit() > max_credit) {
                max_credit = queues[j]->get_credit();
                max_idx = j;
            }
        }
        if (max_idx != -1) {
            Packet p = queues[max_idx]->dequeue();
            queues[max_idx]->spend_credit(service_rate * 0.001);
            std::cout << "Packet dequeued, size: " << p.size << ", weight: " << weights[max_idx] 
                      << ", flow_id: " << p.flow_id << ", credit: " << queues[max_idx]->get_credit() << std::endl;
        }
        current_time += 0.001;
    }

    // Drain remaining queues
    for (int queue_idx = 0; queue_idx < 5; queue_idx++) {
        while (queues[queue_idx]->size() > 0) {
            Packet p = queues[queue_idx]->dequeue();
            queues[queue_idx]->spend_credit(service_rate * 0.001);
            std::cout << "Packet dequeued, size: " << p.size << ", weight: " << weights[queue_idx] 
                      << ", flow_id: " << p.flow_id << ", credit: " << queues[queue_idx]->get_credit() << std::endl;
        }
    }

    std::cout << "Final queue length: 0" << std::endl;
    std::cout << "Packets dropped: " << dropped << std::endl;
    std::cout << "Final queue: empty" << std::endl;

    for (int i = 0; i < 5; i++) {
        delete queues[i];
    }
}

int main() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> size_dist(1, 100);
    std::uniform_int_distribution<> weight_dist(1, 5);
    std::uniform_int_distribution<> flow_dist(1, 5);
    std::vector<Packet> packets(200);
    for (auto& p : packets) {
        p.size = size_dist(gen);
        p.weight = weight_dist(gen);
        p.flow_id = flow_dist(gen);
    }
    simulate_cbfq(packets, 100, 1000, 1.0);
    return 0;
}
