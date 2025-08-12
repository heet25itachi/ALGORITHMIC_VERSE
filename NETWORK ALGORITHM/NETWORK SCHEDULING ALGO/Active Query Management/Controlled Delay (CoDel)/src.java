import java.util.*;

public class CoDel {
    static class Packet {
        int size;
        double arrivalTime;
        Packet(int size, double arrivalTime) {
            this.size = size;
            this.arrivalTime = arrivalTime;
        }
    }

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

    static void simulateCoDel(Packet[] packets, double target, double interval, int capacity) {
        Queue q = new Queue(capacity);
        double currentTime = 0.0;
        double firstAboveTime = 0.0;
        double dropNext = Double.POSITIVE_INFINITY;
        int dropCount = 0;
        int dropped = 0;
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            p.arrivalTime = currentTime;
            if (q.enqueue(p)) {
                while (q.size() > 0) {
                    Packet front = q.items.peekFirst();
                    double sojournTime = currentTime - front.arrivalTime;
                    if (sojournTime < target || q.size() <= 4) {
                        firstAboveTime = 0.0;
                        dropNext = Double.POSITIVE_INFINITY;
                        q.items.pollFirst();
                        System.out.printf("Packet dequeued, size: %d, sojourn time: %.2f\n", front.size, sojournTime);
                        dropCount = 0;
                    } else if (firstAboveTime == 0.0) {
                        firstAboveTime = currentTime + interval;
                        dropNext = firstAboveTime;
                        q.items.pollFirst();
                        System.out.printf("Packet dequeued, size: %d, sojourn time: %.2f\n", front.size, sojournTime);
                    } else if (currentTime >= dropNext) {
                        q.items.pollFirst();
                        System.out.printf("Packet dropped, size: %d, sojourn time: %.2f\n", front.size, sojournTime);
                        dropped++;
                        dropCount++;
                        dropNext = currentTime + interval / Math.sqrt(dropCount);
                    } else {
                        q.items.pollFirst();
                        System.out.printf("Packet dequeued, size: %d, sojourn time: %.2f\n", front.size, sojournTime);
                        dropCount = 0;
                    }
                }
            } else {
                dropped++;
            }
            currentTime += 1.0;
        }

        while (q.size() > 0) {
            Packet front = q.items.peekFirst();
            double sojournTime = currentTime - front.arrivalTime;
            if (sojournTime < target || q.size() <= 4) {
                firstAboveTime = 0.0;
                dropNext = Double.POSITIVE_INFINITY;
                q.items.pollFirst();
                System.out.printf("Packet dequeued, size: %d, sojourn time: %.2f\n", front.size, sojournTime);
                dropCount = 0;
            } else if (firstAboveTime == 0.0) {
                firstAboveTime = currentTime + interval;
                dropNext = firstAboveTime;
                q.items.pollFirst();
                System.out.printf("Packet dequeued, size: %d, sojourn time: %.2f\n", front.size, sojournTime);
            } else if (currentTime >= dropNext) {
                q.items.pollFirst();
                System.out.printf("Packet dropped, size: %d, sojourn time: %.2f\n", front.size, sojournTime);
                dropped++;
                dropCount++;
                dropNext = currentTime + interval / Math.sqrt(dropCount);
            } else {
                q.items.pollFirst();
                System.out.printf("Packet dequeued, size: %d, sojourn time: %.2f\n", front.size, sojournTime);
                dropCount = 0;
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
        double target = 5, interval = 100;
        Packet[] packets = new Packet[n];
        for (int i = 0; i < n; i++) packets[i] = new Packet(rand.nextInt(100) + 1, 0.0);

        simulateCoDel(packets, target, interval, capacity);
    }
}
