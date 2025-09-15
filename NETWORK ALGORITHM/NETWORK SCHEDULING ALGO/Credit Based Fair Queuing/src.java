import java.util.*;

class Packet {
    int size, weight, flowId;
    Packet(int size, int weight, int flowId) {
        this.size = size;
        this.weight = weight;
        this.flowId = flowId;
    }
}

class Queue {
    Queue<Integer> items = new LinkedList<>();
    int capacity;
    double credit, creditRate;
    int totalBandwidth;
    Queue(int capacity, double creditRate, int totalBandwidth) {
        this.capacity = capacity;
        this.creditRate = creditRate;
        this.totalBandwidth = totalBandwidth;
        this.credit = 0.0;
    }
    boolean enqueue(Packet p) {
        if (items.size() >= capacity) return false;
        items.add(p);
        return true;
    }
    Packet dequeue() {
        return (Packet) items.poll();
    }
    void updateCredit(double deltaTime) {
        credit += creditRate * deltaTime;
        if (credit < 0) credit = 0.0;
    }
    double getCredit() { return credit; }
    void spendCredit(double amount) { credit -= amount; if (credit < 0) credit = 0.0; }
    int size() { return items.size(); }
}

public class CBFQ {
    public static void simulateCbfq(Packet[] packets, int capacity, int bandwidth, double baseRate) {
        Queue[] queues = new Queue[5];
        double[] weights = {1.0, 2.0, 3.0, 4.0, 5.0};
        for (int i = 0; i < 5; i++) {
            queues[i] = new Queue(capacity / 5, weights[i] * baseRate, bandwidth / 5);
        }
        double currentTime = 0.0;
        double serviceRate = 1000.0;
        int dropped = 0;
        System.out.println("=== CBFQ Scheduler ===");
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            int queueIdx = p.flowId % 5;
            queues[queueIdx].updateCredit(0.001);

            if (queues[queueIdx].enqueue(p)) {
                System.out.printf("Packet enqueued, size: %d, weight: %d, flow_id: %d, credit: %.2f\n", 
                        p.size, weights[queueIdx], p.flowId, queues[queueIdx].getCredit());
            } else {
                System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
                dropped++;
            }

            // Service: select queue with highest credit
            double maxCredit = -1.0;
            int maxIdx = -1;
            for (int j = 0; j < 5; j++) {
                if (queues[j].size() > 0 && queues[j].getCredit() > maxCredit) {
                    maxCredit = queues[j].getCredit();
                    maxIdx = j;
                }
            }
            if (maxIdx != -1) {
                Packet p = queues[maxIdx].dequeue();
                queues[maxIdx].spendCredit(serviceRate * 0.001);
                System.out.printf("Packet dequeued, size
