import java.util.ArrayList;
import java.util.Random;

// Define Packet and Queue classes
class Packet {
    int size;
    Packet(int size) { this.size = size; }
}

class Queue {
    ArrayList<Packet> items = new ArrayList<>();
    int capacity;
    Queue(int capacity) { this.capacity = capacity; }
    boolean enqueue(Packet p) {
        if (items.size() >= capacity) return false;
        items.add(p);
        return true;
    }
    int size() { return items.size(); }
    void printQueue() {
        System.out.print("Final queue: ");
        for (Packet p : items) System.out.print(p.size + " ");
        System.out.println();
    }
}

public class RED {
    public static void simulateRed(Packet[] packets, double minTh, double maxTh, double wq, double maxP, int capacity) {
        Queue q = new Queue(capacity);
        double avg = 0, count = 0, dropped = 0;
        Random rand = new Random(42);
        System.out.println("Initial queue: empty");
        for (Packet p : packets) {
            avg = q.size() == 0 ? 0 : (1 - wq) * avg + wq * q.size();
            boolean drop = false;
            if (avg < minTh) {
                drop = false;
            } else if (avg >= maxTh) {
                drop = true;
            } else {
                double pb = maxP * (avg - minTh) / (maxTh - minTh);
                double pa = pb / (1 - count * pb);
                count++;
                drop = rand.nextDouble() < pa;
            }
            if (drop) {
                System.out.printf("Packet dropped, size: %d, avg queue length: %.2f, max_p: %.4f\n", p.size, avg, maxP);
                dropped++;
            } else if (q.enqueue(p)) {
                System.out.printf("Packet enqueued, size: %d, avg queue length: %.2f, max_p: %.4f\n", p.size, avg, maxP);
                count = 0;
            } else {
                System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
                dropped++;
            }
        }
        System.out.printf("Final queue length: %d\n", q.size());
        System.out.printf("Packets dropped: %d\n", (int)dropped);
        q.printQueue();
    }

    public static void main(String[] args) {
        Random rand = new Random(42);
        Packet[] packets = new Packet[200];
        for (int i = 0; i < 200; i++) packets[i] = new Packet(rand.nextInt(100) + 1);
        System.out.println("=== RED ===");
        simulateRed(packets, 20, 80, 0.002, 0.1, 100);
    }
}
