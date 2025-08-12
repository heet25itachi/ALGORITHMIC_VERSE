import java.util.ArrayDeque;
import java.util.Random;

// Define Packet and Queue classes
class Packet {
    int size;
    double arrivalTime;
    Packet(int size) { this.size = size; }
}

class Queue {
    ArrayDeque<Packet> items = new ArrayDeque<>();
    int capacity;
    Queue(int capacity) { this.capacity = capacity; }
    boolean enqueue(Packet p) {
        if (items.size() >= capacity) return false;
        items.addLast(p);
        return true;
    }
    Packet dequeue() { return items.removeFirst(); }
    Packet peek() { return items.getFirst(); }
    int size() { return items.size(); }
}

public class PIE {
    public static void simulatePie(Packet[] packets, double target, double updateInterval, double alpha, double beta, double maxDropProb, double maxBurst, int capacity) {
        Queue q = new Queue(capacity);
        double currentTime = 0.0, lastUpdate = 0.0, dropProb = 0.0, prevDelay = 0.0, burstTime = maxBurst;
        int dropped = 0;
        Random rand = new Random(42);
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            p.arrivalTime = currentTime;
            double delay = q.size() == 0 ? 0.0 : currentTime - q.peek().arrivalTime;

            if (currentTime - lastUpdate >= updateInterval) {
                double error = delay - target;
                dropProb += alpha * error + beta * (delay - prevDelay);
                if (dropProb < 0) dropProb = 0;
                if (dropProb > maxDropProb) dropProb = maxDropProb;
                prevDelay = delay;
                lastUpdate = currentTime;
                if (delay > target) burstTime = 0;
                else if (burstTime < maxBurst) burstTime += updateInterval;
            }

            boolean drop = burstTime < maxBurst && delay > target && rand.nextDouble() < dropProb;

            if (drop) {
                System.out.printf("Packet dropped, size: %d, queue delay: %.2f, drop prob: %.4f\n", p.size, delay, dropProb);
                dropped++;
            } else if (q.enqueue(p)) {
                System.out.printf("Packet enqueued, size: %d, queue delay: %.2f, drop prob: %.4f\n", p.size, delay, dropProb);
                Packet deqP = q.dequeue();
                System.out.printf("Packet dequeued, size: %d, queue delay: %.2f\n", deqP.size, delay);
            } else {
                System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
                dropped++;
            }
            currentTime += 1.0;
        }

        while (q.size() > 0) {
            double delay = currentTime - q.peek().arrivalTime;
            if (currentTime - lastUpdate >= updateInterval) {
                double error = delay - target;
                dropProb += alpha * error + beta * (delay - prevDelay);
                if (dropProb < 0) dropProb = 0;
                if (dropProb > maxDropProb) dropProb = maxDropProb;
                prevDelay = delay;
                lastUpdate = currentTime;
                if (delay > target) burstTime = 0;
                else if (burstTime < maxBurst) burstTime += updateInterval;
            }
            Packet p = q.dequeue();
            System.out.printf("Packet dequeued, size: %d, queue delay: %.2f\n", p.size, delay);
            currentTime += 1.0;
        }

        System.out.printf("Final queue length: %d\n", q.size());
        System.out.printf("Packets dropped: %d\n", dropped);
        System.out.println("Final queue: empty");
    }

    public static void main(String[] args) {
        Random rand = new Random(42);
        Packet[] packets = new Packet[200];
        for (int i = 0; i < 200; i++) packets[i] = new Packet(rand.nextInt(100) + 1);
        System.out.println("=== PIE ===");
        simulatePie(packets, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100);
    }
}
