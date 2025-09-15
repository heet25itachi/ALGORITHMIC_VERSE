import java.util.*;

class Packet {
    int size, priority, flowId;
    Packet(int size, int priority, int flowId) {
        this.size = size;
        this.priority = priority;
        this.flowId = flowId;
    }
}

class Queue {
    LinkedList<Packet> items = new LinkedList<>();
    int capacity, totalBandwidth;
    Queue(int capacity, int totalBandwidth) {
        this.capacity = capacity;
        this.totalBandwidth = totalBandwidth;
    }
    boolean enqueue(Packet p) {
        if (items.size() >= capacity) return false;
        items.addLast(p);
        return true;
    }
    Packet dequeue() {
        return items.removeFirst();
    }
    int size() { return items.size(); }
    int bandwidth() { return totalBandwidth; }
}

class PRIQ {
    Queue[] queues;
    PRIQ(int numQueues, int capacity, int bandwidth) {
        queues = new Queue[numQueues];
        for (int i = 0; i < numQueues; i++) {
            queues[i] = new Queue(capacity / numQueues, bandwidth / numQueues);
        }
    }
    void enqueue(Packet p) {
        int pri = p.priority % queues.length;
        if (queues[pri].enqueue(p)) {
            System.out.printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", p.size, pri, p.flowId);
        } else {
            System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
        }
    }
    void dequeueAll() {
        for (int pri = queues.length - 1; pri >= 0; pri--) {
            while (!queues[pri].items.isEmpty()) {
                Packet p = queues[pri].dequeue();
                System.out.printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, pri, p.flowId);
            }
        }
    }
    int totalDropped() {
        int dropped = 0;
        for (Queue q : queues) {
            dropped += q.capacity - q.size();
        }
        return dropped;
    }
}

class CoDelQueue {
    Queue q;
    double targetDelay, interval, firstAboveTime, dropNext;
    int dropCount;
    CoDelQueue(int capacity, int bandwidth, double target, double interval) {
        q = new Queue(capacity, bandwidth);
        targetDelay = target;
        interval = interval;
        firstAboveTime = 0.0;
        dropNext = Double.POSITIVE_INFINITY;
        dropCount = 0;
    }
    boolean enqueue(Packet p) {
        if (q.enqueue(p)) {
            System.out.printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId);
            processQueue(0.0); // Current time
            return true;
        } else {
            System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
            return false;
        }
    }
    void processQueue(double currentTime) {
        while (!q.items.isEmpty()) {
            Packet front = q.items.peek();
            double sojournTime = currentTime - front.arrivalTime; // Assume arrivalTime in Packet
            if (sojournTime < targetDelay || q.size() <= 4) {
                Packet p = q.dequeue();
                System.out.printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId);
                firstAboveTime = 0.0;
                dropNext = Double.POSITIVE_INFINITY;
                dropCount = 0;
            } else if (firstAboveTime == 0.0) {
                firstAboveTime = currentTime + interval;
                dropNext = firstAboveTime;
                Packet p = q.dequeue();
                System.out.printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId);
            } else if (currentTime >= dropNext) {
                Packet p = q.dequeue();
                System.out.printf("Packet dropped, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId);
                dropCount++;
                dropNext = currentTime + interval / Math.sqrt(dropCount);
            } else {
                Packet p = q.dequeue();
                System.out.printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId);
                dropCount = 0;
            }
        }
    }
    int size() { return q.size(); }
}

class CBQ {
    Queue[] nodes;
    CBQ(int numNodes, int capacity, int bandwidth) {
        nodes = new Queue[numNodes];
        for (int i = 0; i < numNodes; i++) {
            nodes[i] = new Queue(capacity / numNodes, bandwidth / numNodes);
        }
    }
    void enqueue(Packet p) {
        int nodeIdx = p.flowId % nodes.length;
        if (nodes[nodeIdx].enqueue(p)) {
            System.out.printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId);
        } else {
            System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
        }
    }
    void dequeueAll() {
        for (int nodeIdx = 0; nodeIdx < nodes.length; nodeIdx++) {
            while (!nodes[nodeIdx].items.isEmpty()) {
                Packet p = nodes[nodeIdx].dequeue();
                System.out.printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId);
            }
        }
    }
}

class FairQ {
    Queue[] flowQueues;
    FairQ(int numFlows, int capacity, int bandwidth) {
        flowQueues = new Queue[numFlows];
        for (int i = 0; i < numFlows; i++) {
            flowQueues[i] = new Queue(capacity / numFlows, bandwidth / numFlows);
        }
    }
    void enqueue(Packet p) {
        int flow = p.flowId % flowQueues.length;
        if (flowQueues[flow].enqueue(p)) {
            System.out.printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, flow);
        } else {
            System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
        }
    }
    void dequeueAll() {
        for (int flow = 0; flow < flowQueues.length; flow++) {
            while (!flowQueues[flow].items.isEmpty()) {
                Packet p = flowQueues[flow].dequeue();
                System.out.printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, flow);
            }
        }
    }
}

class HFSC {
    Queue[] nodes;
    HFSC(int numNodes, int capacity, int bandwidth) {
        nodes = new Queue[numNodes];
        for (int i = 0; i < numNodes; i++) {
            nodes[i] = new Queue(capacity / numNodes, bandwidth / numNodes);
        }
    }
    void enqueue(Packet p) {
        int nodeIdx = p.priority % nodes.length;
        if (nodes[nodeIdx].enqueue(p)) {
            System.out.printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId);
        } else {
            System.out.printf("Queue full, packet dropped, size: %d\n", p.size);
        }
    }
    void dequeueAll() {
        for (int nodeIdx = 0; nodeIdx < nodes.length; nodeIdx++) {
            while (!nodes[nodeIdx].items.isEmpty()) {
                Packet p = nodes[nodeIdx].dequeue();
                System.out.printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId);
            }
        }
    }
}

public class ALTQ {
    public static void simulateAltq(Packet[] packets, int n, int capacity, int bandwidth) {
        System.out.println("=== ALTQ Schedulers Simulation ===");
        simulatePriq(packets, n, capacity, bandwidth);
        simulateCoDel(packets, n, capacity, bandwidth, 5.0, 100.0);
        simulateCbq(packets, n, capacity, bandwidth);
        simulateFairq(packets, n, capacity, bandwidth);
        simulateHfsc(packets, n, capacity, bandwidth);
    }

    private static void simulatePriq(Packet[] packets, int n, int capacity, int bandwidth) {
        PRIQ priq = new PRIQ(16, capacity, bandwidth);
        int dropped = 0;
        System.out.println("=== PRIQ Scheduler ===");
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            priq.enqueue(p);
        }

        priq.dequeueAll();
        System.out.println("Final queue length: 0");
        System.out.println("Packets dropped: " + priq.totalDropped());
        System.out.println("Final queue: empty");
    }

    private static void simulateCoDel(Packet[] packets, int n, int capacity, int bandwidth, double target, double interval) {
        CoDelQueue codel = new CoDelQueue(capacity, bandwidth, target, interval);
        int dropped = 0;
        double currentTime = 0.0;
        System.out.println("=== CoDel Scheduler ===");
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            if (codel.enqueue(p)) {
                // Process queue
            } else {
                dropped++;
            }
            currentTime += 1.0;
        }

        System.out.println("Final queue length: " + codel.size());
        System.out.println("Packets dropped: " + dropped);
        System.out.println("Final queue: empty");
    }

    // Similar implementations for CBQ, FairQ, HFSC...
    private static void simulateCbq(Packet[] packets, int n, int capacity, int bandwidth) {
        CBQ cbq = new CBQ(4, capacity, bandwidth);
        int dropped = 0;
        System.out.println("=== CBQ Scheduler ===");
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            cbq.enqueue(p);
        }

        cbq.dequeueAll();
        System.out.println("Final queue length: 0");
        System.out.println("Packets dropped: " + dropped);
        System.out.println("Final queue: empty");
    }

    private static void simulateFairq(Packet[] packets, int n, int capacity, int bandwidth) {
        FairQ fairq = new FairQ(5, capacity, bandwidth);
        int dropped = 0;
        System.out.println("=== FairQ Scheduler ===");
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            fairq.enqueue(p);
        }

        fairq.dequeueAll();
        System.out.println("Final queue length: 0");
        System.out.println("Packets dropped: " + dropped);
        System.out.println("Final queue: empty");
    }

    private static void simulateHfsc(Packet[] packets, int n, int capacity, int bandwidth) {
        HFSC hfsc = new HFSC(4, capacity, bandwidth);
        int dropped = 0;
        System.out.println("=== HFSC Scheduler ===");
        System.out.println("Initial queue: empty");

        for (Packet p : packets) {
            hfsc.enqueue(p);
        }

        hfsc.dequeueAll();
        System.out.println("Final queue length: 0");
        System.out.println("Packets dropped: " + dropped);
        System.out.println("Final queue: empty");
    }

    public static void main(String[] args) {
        Random rand = new Random(42);
        Packet[] packets = new Packet[200];
        for (int i = 0; i < 200; i++) {
            packets[i] = new Packet(rand.nextInt(100) + 1, rand.nextInt(16), rand.nextInt(5));
        }
        simulateAltq(packets, 200, 100, 1000);
    }
}
