#include <iostream>
#include <vector>
#include <queue>
#include <random>

struct Packet {
    int size, priority, flow_id;
};

class Queue {
    std::queue<Packet> items;
    int capacity, total_bandwidth;
public:
    Queue(int cap, int bw) : capacity(cap), total_bandwidth(bw) {}
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
    int size() { return items.size(); }
    int bandwidth() { return total_bandwidth; }
};

class PRIQ {
    std::vector<Queue*> queues;
public:
    PRIQ(int num_queues, int capacity, int bandwidth) {
        queues.resize(num_queues);
        for (int i = 0; i < num_queues; i++) {
            queues[i] = new Queue(capacity / num_queues, bandwidth / num_queues);
        }
    }
    ~PRIQ() {
        for (auto q : queues) delete q;
    }
    void enqueue(Packet p) {
        int pri = p.priority % queues.size();
        if (queues[pri]->enqueue(p)) {
            std::cout << "Packet enqueued, size: " << p.size << ", priority: " << pri << ", flow_id: " << p.flow_id << std::endl;
        } else {
            std::cout << "Queue full, packet dropped, size: " << p.size << std::endl;
        }
    }
    void dequeue_all() {
        for (int pri = queues.size() - 1; pri >= 0; pri--) {
            while (!queues[pri]->items.empty()) {
                Packet p = queues[pri]->dequeue();
                std::cout << "Packet dequeued, size: " << p.size << ", priority: " << pri << ", flow_id: " << p.flow_id << std::endl;
            }
        }
    }
    int total_dropped() {
        int dropped = 0;
        for (auto q : queues) {
            dropped += q->capacity - q->size(); // Simplified
        }
        return dropped;
    }
};

class CoDelQueue {
    Queue q;
    double target_delay, interval, first_above_time, drop_next;
    int drop_count;
public:
    CoDelQueue(int capacity, int bandwidth, double target, double interval) : q(capacity, bandwidth), target_delay(target), interval(interval), first_above_time(0), drop_next(INFINITY), drop_count(0) {}
    bool enqueue(Packet p) {
        if (q.enqueue(p)) {
            std::cout << "Packet enqueued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << p.flow_id << std::endl;
            process_queue(0.0); // Current time
            return true;
        } else {
            std::cout << "Queue full, packet dropped, size: " << p.size << std::endl;
            return false;
        }
    }
    void process_queue(double current_time) {
        while (!q.items.empty()) {
            double sojourn_time = current_time - q.items.front().arrival_time; // Assume arrival_time in Packet
            if (sojourn_time < target_delay || q.size() <= 4) {
                Packet p = q.dequeue();
                std::cout << "Packet dequeued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << p.flow_id << std::endl;
                first_above_time = 0;
                drop_next = INFINITY;
                drop_count = 0;
            } else if (first_above_time == 0) {
                first_above_time = current_time + interval;
                drop_next = first_above_time;
                Packet p = q.dequeue();
                std::cout << "Packet dequeued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << p.flow_id << std::endl;
            } else if (current_time >= drop_next) {
                Packet p = q.dequeue();
                std::cout << "Packet dropped, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << p.flow_id << std::endl;
                drop_count++;
                drop_next = current_time + interval / sqrt(drop_count);
            } else {
                Packet p = q.dequeue();
                std::cout << "Packet dequeued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << p.flow_id << std::endl;
                drop_count = 0;
            }
        }
    }
    int size() { return q.size(); }
};

class CBQ {
    std::vector<CBQNode*> nodes;
public:
    CBQ(int num_nodes, int capacity, int bandwidth) {
        nodes.resize(num_nodes);
        for (int i = 0; i < num_nodes; i++) {
            nodes[i] = new CBQNode(capacity / num_nodes, bandwidth / num_nodes);
        }
    }
    ~CBQ() {
        for (auto node : nodes) delete node;
    }
    void enqueue(Packet p) {
        int node_idx = p.flow_id % nodes.size();
        if (nodes[node_idx]->enqueue(p)) {
            std::cout << "Packet enqueued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << p.flow_id << std::endl;
        } else {
            std::cout << "Queue full, packet dropped, size: " << p.size << std::endl;
        }
    }
    void dequeue_all() {
        for (auto node : nodes) {
            while (!node->queue->items.empty()) {
                Packet p = node->queue->dequeue();
                std::cout << "Packet dequeued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << p.flow_id << std::endl;
            }
        }
    }
};

struct CBQNode {
    Queue *queue;
    double bandwidth;
    CBQNode(int capacity, int bandwidth) : bandwidth(bandwidth) {
        queue = new Queue(capacity, bandwidth);
    }
    ~CBQNode() { delete queue; }
    bool enqueue(Packet p) {
        return queue->enqueue(p);
    }
};

class FairQ {
    std::vector<Queue*> flow_queues;
public:
    FairQ(int num_flows, int capacity, int bandwidth) {
        flow_queues.resize(num_flows);
        for (int i = 0; i < num_flows; i++) {
            flow_queues[i] = new Queue(capacity / num_flows, bandwidth / num_flows);
        }
    }
    ~FairQ() {
        for (auto q : flow_queues) delete q;
    }
    void enqueue(Packet p) {
        int flow = p.flow_id % flow_queues.size();
        if (flow_queues[flow]->enqueue(p)) {
            std::cout << "Packet enqueued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << flow << std::endl;
        } else {
            std::cout << "Queue full, packet dropped, size: " << p.size << std::endl;
        }
    }
    void dequeue_all() {
        for (int flow = 0; flow < flow_queues.size(); flow++) {
            while (!flow_queues[flow]->items.empty()) {
                Packet p = flow_queues[flow]->dequeue();
                std::cout << "Packet dequeued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << flow << std::endl;
            }
        }
    }
};

class HFSC {
    std::vector<Queue*> nodes;
public:
    HFSC(int num_nodes, int capacity, int bandwidth) {
        nodes.resize(num_nodes);
        for (int i = 0; i < num_nodes; i++) {
            nodes[i] = new Queue(capacity / num_nodes, bandwidth / num_nodes);
        }
    }
    ~HFSC() {
        for (auto node : nodes) delete node;
    }
    void enqueue(Packet p) {
        int node_idx = p.priority % nodes.size();
        if (nodes[node_idx]->enqueue(p)) {
            std::cout << "Packet enqueued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << p.flow_id << std::endl;
        } else {
            std::cout << "Queue full, packet dropped, size: " << p.size << std::endl;
        }
    }
    void dequeue_all() {
        for (int node_idx = 0; node_idx < nodes.size(); node_idx++) {
            while (!nodes[node_idx]->items.empty()) {
                Packet p = nodes[node_idx]->dequeue();
                std::cout << "Packet dequeued, size: " << p.size << ", priority: " << p.priority << ", flow_id: " << p.flow_id << std::endl;
            }
        }
    }
};

void simulate_altq(std::vector<Packet> packets, int n, int capacity, int bandwidth) {
    std::cout << "=== ALTQ Schedulers Simulation ===\n";
    simulate_priq(packets, n, capacity, bandwidth);
    simulate_codel(packets, n, capacity, bandwidth, 5.0, 100.0);
    simulate_cbq(packets, n, capacity, bandwidth);
    simulate_fairq(packets, n, capacity, bandwidth);
    simulate_hfsc(packets, n, capacity, bandwidth);
}

int main() {
    srand(time(NULL));
    std::vector<Packet> packets(200);
    for (int i = 0; i < 200; i++) {
        packets[i].size = rand() % 100 + 1;
        packets[i].priority = rand() % 16;
        packets[i].flow_id = rand() % 5;
    }

    simulate_altq(packets, 200, 100, 1000);
    return 0;
}
