import java.util.*;

public class AQM {
    static class Packet { int size; double arrivalTime; Packet(int size) { this.size = size; } }

    static class Queue {
        Deque<Packet> items = new LinkedList<>();
        int capacity;
        Queue(int cap) { capacity = cap; }
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

    static void simulateARED(Packet[] packets, double minTh, double maxTh, double wq, double target, double alpha, double beta, double interval, int capacity) {
        Queue q = new Queue(capacity);
        double avg = 0, maxP = 0.1, lastUpdate = 0, currentTime = 0;
        int count = 0, dropped = 0;
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            avg = q.size() == 0 ? 0 : (1 - wq) * avg + wq * q.size();
            if (currentTime - lastUpdate >= interval) {
                if (avg > target && maxP <= 0.5) maxP *= (1 + alpha);
                else if (avg < target && maxP >= 0.01) maxP *= beta;
                lastUpdate = currentTime;
            }

            boolean drop = false;
            if (avg < minTh) drop = false;
            else if (avg >= maxTh) drop = true;
            else {
                double pb = maxP * (avg - minTh) / (maxTh - minTh);
                double pa = pb / (1 - count * pb);
                drop = Math.random() < pa;
                count++;
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
            currentTime += 1.0;
        }

        System.out.println("Final queue length: " + q.size());
        System.out.println("Packets dropped: " + dropped);
        q.printQueue();
    }

    static void simulateBlue(Packet[] packets, double d1, double d2, double freezeTime, int capacity) {
        Queue q = new Queue(capacity);
        double p = 0, lastUpdate = 0, currentTime = 0;
        int dropped = 0;
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            if (q.size() >= capacity) {
                p += d1;
                lastUpdate = currentTime;
                System.out.printf("Queue full, packet dropped, size: %d, drop prob: %.4f\n", p.size, p);
                dropped++;
            } else {
                if (currentTime - lastUpdate >= freezeTime && q.size() == 0) {
                    p = p > d2 ? p - d2 : 0;
                    lastUpdate = currentTime;
                }
                if (Math.random() < p) {
                    System.out.printf("Packet dropped, size: %d, drop prob: %.4f\n", p.size, p);
                    dropped++;
                } else if (q.enqueue(p)) {
                    System.out.printf("Packet enqueued, size: %d, drop prob: %.4f\n", p.size, p);
                }
            }
            currentTime += 1.0;
        }

        System.out.println("Final queue length: " + q.size());
        System.out.println("Packets dropped: " + dropped);
        q.printQueue();
    }

    static void simulatePI(Packet[] packets, double qRef, double a, double b, int capacity) {
        Queue q = new Queue(capacity);
        double p = 0, prevError = 0, currentTime = 0;
        int dropped = 0;
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            double error = q.size() - qRef;
            p += a * error - b * prevError;
            prevError = error;
            if (p < 0) p = 0; else if (p > 1) p = 1;

            if (Math.random() < p) {
                System.out.printf("Packet dropped, size: %d, drop prob: %.4f\n", p.size, p);
                dropped++;
            } else if (q.enqueue(p)) {
                System.out.printf("Packet enqueued, size: %d, drop prob: %.4f\n", p.size, p);
            } else {
                System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
                dropped++;
            }
            currentTime += 1.0;
        }

        System.out.println("Final queue length: " + q.size());
        System.out.println("Packets dropped: " + dropped);
        q.printQueue();
    }

    public static void main(String[] args) {
        Random rand = new Random();
        int n = 200, capacity = 100;
        Packet[] packets = new Packet[n];
        for (int i = 0; i < n; i++) packets[i] = new Packet(rand.nextInt(100) + 1);

        System.out.println("=== ARED ===");
        simulateARED(packets, 20, 80, 0.002, 50, 0.01, 0.9, 1000, capacity);
        System.out.println("\n=== Blue ===");
        simulateBlue(packets, 0.0002, 0.00005, 100, capacity);
        System.out.println("\n=== PI ===");
        simulatePI(packets, 50, 0.00001822, 0.00001816, capacity);
    }
}
