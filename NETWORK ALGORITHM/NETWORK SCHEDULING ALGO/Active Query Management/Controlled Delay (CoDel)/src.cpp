#include <iostream>
#include <deque>
#include <cstdlib>
#include <ctime>
#include <cmath>
using namespace std;

struct Packet {
    int size;
    double arrival_time;
};

class CoDelQueue {
    deque<Packet> items;
    int capacity;
    double first_above_time;
    double drop_next;
    int drop_count;

public:
    CoDelQueue(int cap) : capacity(cap), first_above_time(0), drop_next(INFINITY), drop_count(0) {}

    bool enqueue(Packet p) {
        if (items.size() >= capacity) {
            cout << "Queue full, packet dropped, size: " << p.size << endl;
            return false;
        }
        items.push_back(p);
        cout << "Packet enqueued, size: " << p.size << endl;
        return true;
    }

    void dequeue(double current_time, double target, double interval) {
        while (items.size() > 0) {
            Packet p = items.front();
            double sojourn_time = current_time - p.arrival_time;

            if (sojourn_time < target || items.size() <= 4) {
                first_above_time = 0;
                drop_next = INFINITY;
                items.pop_front();
                cout << "Packet dequeued, size: " << p.size << ", sojourn time: " << sojourn_time << endl;
                drop_count = 0;
            } else if (first_above_time == 0) {
                first_above_time = current_time + interval;
                drop_next = first_above_time;
                items.pop_front();
                cout << "Packet dequeued, size: " << p.size << ", sojourn time: " << sojourn_time << endl;
            } else if (current_time >= drop_next) {
                items.pop_front();
                cout << "Packet dropped, size: " << p.size << ", sojourn time: " << sojourn_time << endl;
                drop_count++;
                drop_next = current_time + interval / sqrt(drop_count);
            } else {
                items.pop_front();
                cout << "Packet dequeued, size: " << p.size << ", sojourn time: " << sojourn_time << endl;
                drop_count = 0;
            }
        }
    }

    int size() { return items.size(); }
};

void simulate_codel(vector<Packet> packets, double target, double interval, int capacity) {
    CoDelQueue q(capacity);
    double current_time = 0.0;
    int dropped = 0;
    cout << "Initial queue: empty" << endl;

    for (Packet p : packets) {
        p.arrival_time = current_time;
        if (q.enqueue(p)) {
            q.dequeue(current_time, target, interval);
        } else {
            dropped++;
        }
        current_time += 1.0;
    }

    q.dequeue(current_time, target, interval);

    cout << "Final queue length: " << q.size() << endl;
    cout << "Packets dropped: " << dropped << endl;
    cout << "Final queue: empty" << endl;
}

int main() {
    srand(time(NULL));
    vector<Packet> packets(200);
    for (int i = 0; i < 200; i++) packets[i].size = rand() % 100 + 1;

    simulate_codel(packets, 5, 100, 100);

    return 0;
}
