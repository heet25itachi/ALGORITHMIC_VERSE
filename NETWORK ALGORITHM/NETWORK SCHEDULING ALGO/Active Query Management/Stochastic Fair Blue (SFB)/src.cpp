#include <iostream>
#include <deque>
#include <cstdlib>
#include <ctime>
#include <vector>
using namespace std;

struct Packet {
    int size;
    int flow_id;
};

class Queue {
    deque<Packet> items;
    int capacity;
public:
    Queue(int cap) : capacity(cap) {}
    bool enqueue(Packet p) {
        if (items.size() >= capacity) return false;
        items.push_back(p);
        return true;
    }
    Packet dequeue() {
        Packet p = items.front();
        items.pop_front();
        return p;
    }
    int size() { return items.size(); }
};

class Bin {
    double p;
    double last_update;
public:
    Bin() : p(0.0), last_update(0.0) {}
    double getP() { return p; }
    void updateP(double val) { p = val; }
    double getLastUpdate() { return last_update; }
    void updateLastUpdate(double val) { last_update = val; }
};

class SFB {
    vector<Bin> bins;
    int L, N;
public:
    SFB(int l, int n) : L(l), N(n), bins(l * n) {}
    int drop(int flow_id, double current_time, double d1, double d2, double freeze_time, int queue_length, int capacity) {
        int marked = 1;
        for (int l = 0; l < L; l++) {
            int bin_idx = l * N + (flow_id + l) % N; // Simple hash
            Bin *bin = &bins[bin_idx];
            if (current_time - bin->getLastUpdate() >= freeze_time) {
                if (queue_length == 0) {
                    bin->updateP(max(bin->getP() - d2, 0.0));
                } else if (queue_length >= capacity) {
                    bin->updateP(bin->getP() + d1);
                }
                bin->updateLastUpdate(current_time);
            }
            if (bin->getP() < 1.0) {
                marked = 0;
                break;
            }
        }
        return marked;
    }
};

void simulate_sfb(vector<Packet> packets, double d1, double d2, double freeze_time, int capacity, int L, int N) {
    Queue q(capacity);
    SFB sfb(L, N);
    double current_time = 0.0;
    int dropped = 0;
    cout << "Initial queue: empty" << endl;

    for (Packet p : packets) {
        if (sfb.drop(p.flow_id, current_time, d1, d2, freeze_time, q.size(), capacity)) {
            cout << "Packet dropped, size: " << p.size << ", flow_id: " << p.flow_id << endl;
            dropped++;
        } else if (q.enqueue(p)) {
            cout << "Packet enqueued, size: " << p.size << ", flow_id: " << p.flow_id << endl;
            // Immediate dequeue for simulation
            Packet deq_p = q.dequeue();
            cout << "Packet dequeued, size: " << deq_p.size << ", flow_id: " << deq_p.flow_id << endl;
        } else {
            cout << "Queue full, packet dropped, size: " << p.size << endl;
            dropped++;
        }
        current_time += 1.0;
    }

    while (q.size() > 0) {
        Packet p = q.dequeue();
        cout << "Packet dequeued, size: " << p.size << ", flow_id: " << p.flow_id << endl;
        current_time += 1.0;
    }

    cout << "Final queue length: " << q.size() << endl;
    cout << "Packets dropped: " << dropped << endl;
    cout << "Final queue: empty" << endl;
}

int main() {
    srand(time(NULL));
    vector<Packet> packets(200);
    for (int i = 0; i < 200; i++) {
        packets[i].size = rand() % 100 + 1;
        packets[i].flow_id = rand() % 20 + 1;
    }

    cout << "=== SFB ===\n";
    simulate_sfb(packets, 0.0002, 0.00005, 100, 100, 2, 4);

    return 0;
}
