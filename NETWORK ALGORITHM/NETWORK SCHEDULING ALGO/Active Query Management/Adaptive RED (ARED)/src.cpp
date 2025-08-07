#include <iostream>
#include <deque>
#include <cstdlib>
#include <ctime>
using namespace std;

struct Packet { int size; double arrival_time; };

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
    int size() { return items.size(); }
    void print_queue() {
        cout << "Final queue: ";
        for (const auto& p : items) cout << p.size << " ";
        cout << endl;
    }
};

void simulate_ared(Packet *packets, int n, double min_th, double max_th, double w_q, double target, double alpha, double beta, double interval, int capacity) {
    Queue q(capacity);
    double avg = 0, max_p = 0.1, last_update = 0, current_time = 0;
    int count = 0, dropped = 0;
    cout << "Initial queue: empty" << endl;

    for (int i = 0; i < n; i++) {
        avg = q.size() == 0 ? 0 : (1 - w_q) * avg + w_q * q.size();
        if (current_time - last_update >= interval) {
            if (avg > target && max_p <= 0.5) max_p *= (1 + alpha);
            else if (avg < target && max_p >= 0.01) max_p *= beta;
            last_update = current_time;
        }

        bool drop = false;
        if (avg < min_th) drop = false;
        else if (avg >= max_th) drop = true;
        else {
            double pb = max_p * (avg - min_th) / (max_th - min_th);
            double pa = pb / (1 - count * pb);
            drop = (double)rand() / RAND_MAX < pa;
            count++;
        }

        if (drop) {
            cout << "Packet dropped, size: " << packets[i].size << ", avg queue length: " << avg << ", max_p: " << max_p << endl;
            dropped++;
        } else if (q.enqueue(packets[i])) {
            cout << "Packet enqueued, size: " << packets[i].size << ", avg queue length: " << avg << ", max_p: " << max_p << endl;
            count = 0;
        } else {
            cout << "Queue full, packet dropped, size: " << packets[i].size << endl;
            dropped++;
        }
        current_time += 1.0;
    }

    cout << "Final queue length: " << q.size() << endl;
    cout << "Packets dropped: " << dropped << endl;
    q.print_queue();
}

void simulate_blue(Packet *packets, int n, double d1, double d2, double freeze_time, int capacity) {
    Queue q(capacity);
    double p = 0, last_update = 0, current_time = 0;
    int dropped = 0;
    cout << "Initial queue: empty" << endl;

    for (int i = 0; i < n; i++) {
        if (q.size() >= capacity) {
            p += d1;
            last_update = current_time;
            cout << "Queue full, packet dropped, size: " << packets[i].size << ", drop prob: " << p << endl;
            dropped++;
        } else {
            if (current_time - last_update >= freeze_time && q.size() == 0) {
                p = p > d2 ? p - d2 : 0;
                last_update = current_time;
            }
            if ((double)rand() / RAND_MAX < p) {
                cout << "Packet dropped, size: " << packets[i].size << ", drop prob: " << p << endl;
                dropped++;
            } else if (q.enqueue(packets[i])) {
                cout << "Packet enqueued, size: " << packets[i].size << ", drop prob: " << p << endl;
            }
        }
        current_time += 1.0;
    }

    cout << "Final queue length: " << q.size() << endl;
    cout << "Packets dropped: " << dropped << endl;
    q.print_queue();
}

void simulate_pi(Packet *packets, int n, double q_ref, double a, double b, int capacity) {
    Queue q(capacity);
    double p = 0, prev_error = 0, current_time = 0;
    int dropped = 0;
    cout << "Initial queue: empty" << endl;

    for (int i = 0; i < n; i++) {
        double error = q.size() - q_ref;
        p += a * error - b * prev_error;
        prev_error = error;
        if (p < 0) p = 0; else if (p > 1) p = 1;

        if ((double)rand() / RAND_MAX < p) {
            cout << "Packet dropped, size: " << packets[i].size << ", drop prob: " << p << endl;
            dropped++;
        } else if (q.enqueue(packets[i])) {
            cout << "Packet enqueued, size: " << packets[i].size << ", drop prob: " << p << endl;
        } else {
            cout << "Queue full, packet dropped, size: " << packets[i].size << endl;
            dropped++;
        }
        current_time += 1.0;
    }

    cout << "Final queue length: " << q.size() << endl;
    cout << "Packets dropped: " << dropped << endl;
    q.print_queue();
}

int main() {
    srand(time(NULL));
    int n = 200, capacity = 100;
    Packet *packets = new Packet[n];
    for (int i = 0; i < n; i++) packets[i].size = rand() % 100 + 1;

    cout << "=== ARED ===" << endl;
    simulate_ared(packets, n, 20, 80, 0.002, 50, 0.01, 0.9, 1000, capacity);
    cout << "\n=== Blue ===" << endl;
    simulate_blue(packets, n, 0.0002, 0.00005, 100, capacity);
    cout << "\n=== PI ===" << endl;
    simulate_pi(packets, n, 50, 0.00001822, 0.00001816, capacity);

    delete[] packets;
    return 0;
}
