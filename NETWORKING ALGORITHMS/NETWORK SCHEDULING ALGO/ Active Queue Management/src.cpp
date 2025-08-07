#include <iostream>
#include <queue>
#include <cstdlib>
#include <ctime>
using namespace std;

struct Packet {
    int size;
};

class REDQueue {
    deque<int> items;
    int capacity;
    double avg;
    int count;

public:
    REDQueue(int cap) : capacity(cap), avg(0), count(0) {}

    bool enqueue(Packet p, double min_th, double max_th, double w_q, double max_p) {
        if (items.empty()) avg = 0;
        else avg = (1 - w_q) * avg + w_q * items.size();

        bool drop = false;
        if (avg < min_th) {
            drop = false;
        } else if (avg >= max_th) {
            drop = true;
        } else {
            double pb = max_p * (avg - min_th) / (max_th - min_th);
            double pa = pb / (1 - count * pb);
            if ((double)rand() / RAND_MAX < pa) drop = true;
            else drop = false;
            count++;
        }

        if (drop) {
            cout << "Packet dropped, size: " << p.size << ", avg queue length: " << avg << endl;
            return false;
        } else if (items.size() < capacity) {
            items.push_back(p.size);
            cout << "Packet enqueued, size: " << p.size << ", avg queue length: " << avg << endl;
            count = 0;
            return true;
        } else {
            cout << "Queue full, packet dropped, size: " << p.size << endl;
            return false;
        }
    }

    int size() { return items.size(); }

    void print_queue() {
        cout << "Final queue: ";
        for (int x : items) cout << x << " ";
        cout << endl;
    }
};

void simulate_red(Packet *packets, int n, double min_th, double max_th, double w_q, double max_p, int capacity) {
    REDQueue q(capacity);
    int dropped = 0;
    cout << "Initial queue: empty" << endl;

    for (int i = 0; i < n; i++) {
        if (!q.enqueue(packets[i], min_th, max_th, w_q, max_p)) {
            dropped++;
        }
    }

    cout << "Final queue length: " << q.size() << endl;
    cout << "Packets dropped: " << dropped << endl;
    q.print_queue();
}

int main() {
    srand(time(NULL));
    int n = 200, capacity = 100;
    double min_th = 20, max_th = 80, w_q = 0.002, max_p = 0.1;
    Packet *packets = new Packet[n];
    for (int i = 0; i < n; i++) packets[i].size = rand() % 100 + 1;

    simulate_red(packets, n, min_th, max_th, w_q, max_p, capacity);

    delete[] packets;
    return 0;
}
