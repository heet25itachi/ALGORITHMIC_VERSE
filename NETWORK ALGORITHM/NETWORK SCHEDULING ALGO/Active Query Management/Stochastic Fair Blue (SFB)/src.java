import java.util.*;

public class SFB {
    static class Packet {
        int size;
        int flowId;
        Packet(int size, int flowId) {
            this.size = size;
            this.flowId = flowId;
        }
    }

    static class Bin {
        double p;
        double lastUpdate;
        Bin() { p = 0.0; lastUpdate = 0.0; }
    }

    static class SFB {
        Bin[] bins;
        int L, N;
        SFB(int l, int n) {
            L = l;
            N = n;
            bins = new Bin[l * n];
            for (int i = 0; i < l * n; i++) bins[i] = new Bin();
        }
        int drop(int flowId, double currentTime, double d1, double d2, double freezeTime, int queueLength, int capacity) {
            int marked = 1;
            for (int l = 0; l < L; l++) {
                int binIdx = l * N + (flowId + l) % N; // Simple hash
                Bin bin = bins[binIdx];
                if (currentTime - bin.lastUpdate >= freezeTime) {
                    if (queueLength == 0) bin.p = max(bin.p - d2, 0.0);
                    else if (queueLength >= capacity) bin.p += d1;
                    bin.lastUpdate = currentTime;
                }
                if (bin.p < 1.0) {
                    marked = 0;
                    break;
                }
            }
            return marked;
        }
    }

    static class Queue {
        Deque<Packet> items = new LinkedList<>();
        int capacity;
        Queue(int capacity) { this.capacity = capacity; }
        bool enqueue(Packet p) {
            if (items.size() >= capacity) return false;
            items.addLast(p);
            return true;
        }
        Packet dequeue() { return items.removeFirst(); }
        int size() { return items.size(); }
    }

    static void simulateSFB(Packet[] packets, double d1, double d2, double freezeTime, int capacity, int L, int N) {
        Queue q = new Queue(capacity);
        SFB sfb = new SFB(L, N);
        double currentTime = 0.0;
        int dropped = 0;
        System.out.println("Initial queue: empty");

        Random rand = new Random(42);
        for (Packet p : packets) {
            if (sfb.drop(p.flowId, currentTime, d1, d2, freezeTime, q.size(), capacity) == 1) {
                System.out.println("Packet dropped, size: " + p.size + ", flow_id: " + p.flowId);
                dropped++;
            } else if (q.enqueue(p)) {
                System.out.println("Packet enqueued, size: " + p.size + ", flow_id: " + p.flowId);
                // Immediate dequeue for simulation
                Packet deqP = q.dequeue();
                System.out.println("Packet dequeued, size: " + deqP.size + ", flow_id: " + deqP.flowId);
            } else {
                System.out.println("Queue full, packet dropped, size: " + p.size);
                dropped++;
            }
            currentTime += 1.0;
        }

        while (q.size() > 0) {
            Packet p = q.dequeue();
            System.out.println("Packet dequeued, size: " + p.size + ", flow_id: " + p.flowId);
            currentTime += 1.0;
        }

        System.out.println("Final queue length: " + q.size());
        System.out.println("Packets dropped: " + dropped);
        System.out.println("Final queue: empty");
    }

    public static void main(String[] args) {
        Packet[] packets = new Packet[200];
        Random rand = new Random(42);
        for (int i = 0; i < 200; i++) {
            packets[i] = new Packet(rand.nextInt(100) + 1, rand.nextInt(20) + 1);
        }

        System.out.println("=== SFB ===");
        simulateSFB(packets, 0.0002, 0.00005, 100, 100, 2, 4);
    }
}
