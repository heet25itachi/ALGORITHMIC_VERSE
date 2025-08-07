import java.util.*;

public class REDQueue {
    static class Packet {
        int size;
        Packet(int size) { this.size = size; }
    }

    static class Queue {
        Deque<Integer> items = new LinkedList<>();
        int capacity;
        double avg;
        int count;

        Queue(int cap) {
            capacity = cap;
            avg = 0;
            count = 0;
        }

        boolean enqueue(Packet p, double min_th, double max_th, double w_q, double max_p) {
            if (items.isEmpty()) avg = 0;
            else avg = (1 - w_q) * avg + w_q * items.size();

            boolean drop = false;
            if (avg < min_th) {
                drop = false;
            } else if (avg >= max_th) {
                drop = true;
            } else {
                double pb = max_p * (avg - min_th) / (max_th - min_th);
                double pa = pb / (1 - count * pb);
                if (Math.random() < pa) drop = true;
                else drop = false;
                count++;
            }

            if (drop) {
                System.out.printf("Packet dropped, size: %d, avg queue length: %.2f\n", p.size, avg);
                return false;
            } else if (items.size() < capacity) {
                items.add(p.size);
                System.out.printf("Packet enqueued, size: %d, avg queue length: %.2f\n", p.size, avg);
                count = 0;
                return true;
            } else {
                System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
                return false;
            }
        }

        int size() { return items.size(); }

        void printQueue() {
            System.out.print("Final queue: ");
            for (int x : items) System.out.print(x + " ");
            System.out.println();
        }
    }

    static void simulateRED(Packet[] packets, double min_th, double max_th, double w_q, double max_p, int capacity) {
        Queue q = new Queue(capacity);
        int dropped = 0;
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            if (!q.enqueue(p, min_th, max_th, w_q, max_p)) {
                dropped++;
            }
        }

        System.out.println("Final queue length: " + q.size());
        System.out.println("Packets dropped: " + dropped);
        q.printQueue();
    }

    public static void main(String[] args) {
        Random rand = new Random();
        int n = 200, capacity = 100;
        double min_th = 20, max_th = 80, w_q = 0.002, max_p = 0.1;
        Packet[] packets = new Packet[n];
        for (int i = 0; i < n; i++) packets[i] = new Packet(rand.nextInt(100) + 1);

        simulateRED(packets, min_th, max_th, w_q, max_p, capacity);
    }
}
