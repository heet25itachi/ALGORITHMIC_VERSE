import random
from collections import deque

class Packet:
    def __init__(self, size, priority, flow_id):
        self.size = size
        self.priority = priority
        self.flow_id = flow_id

class Queue:
    def __init__(self, capacity, bandwidth):
        self.items = deque()
        self.capacity = capacity
        self.total_bandwidth = bandwidth

    def enqueue(self, p):
        if len(self.items) >= self.capacity:
            return False
        self.items.append(p)
        return True

    def dequeue(self):
        return self.items.popleft()

    def size(self):
        return len(self.items)

class PRIQ:
    def __init__(self, num_queues, capacity, bandwidth):
        self.queues = [Queue(capacity // num_queues, bandwidth // num_queues) for _ in range(num_queues)]

    def enqueue(self, p):
        pri = p.priority % len(self.queues)
        if self.queues[pri].enqueue(p):
            print(f"Packet enqueued, size: {p.size}, priority: {pri}, flow_id: {p.flow_id}")
        else:
            print(f"Queue full, packet dropped, size: {p.size}")

    def dequeue_all(self):
        for pri in range(len(self.queues) - 1, -1, -1):
            while self.queues[pri].size() > 0:
                p = self.queues[pri].dequeue()
                print(f"Packet dequeued, size: {p.size}, priority: {pri}, flow_id: {p.flow_id}")

class CoDelQueue:
    def __init__(self, capacity, bandwidth, target, interval):
        self.q = Queue(capacity, bandwidth)
        self.target_delay = target
        self.interval = interval
        self.first_above_time = 0.0
        self.drop_next = float('inf')
        self.drop_count = 0

    def enqueue(self, p):
        if self.q.enqueue(p):
            print(f"Packet enqueued, size: {p.size}, priority: {p.priority}, flow_id: {p.flow_id}")
            self.process_queue(0.0)
            return True
        else:
            print(f"Queue full, packet dropped, size: {p.size}")
            return False

    def process_queue(self, current_time):
        while self.q.size() > 0:
            sojourn_time = current_time - self.q.items[0].arrival_time  # Assume arrival_time
            if sojourn_time < self.target_delay or self.q.size() <= 4:
                p = self.q.dequeue()
                print(f"Packet dequeued, size: {p.size}, priority: {p.priority}, flow_id: {p.flow_id}")
                self.first_above_time = 0.0
                self.drop_next = float('inf')
                self.drop_count = 0
            elif self.first_above_time == 0.0:
                self.first_above_time = current_time + self.interval
                self.drop_next = self.first_above_time
                p = self.q.dequeue()
                print(f"Packet dequeued, size: {p.size}, priority: {p.priority}, flow_id: {p.flow_id}")
            elif current_time >= self.drop_next:
                p = self.q.dequeue()
                print(f"Packet dropped, size: {p.size}, priority: {p.priority}, flow_id: {p.flow_id}")
                self.drop_count += 1
                self.drop_next = current_time + self.interval / math.sqrt(self.drop_count)
            else:
                p = self.q.dequeue()
                print(f"Packet dequeued, size: {p.size}, priority: {p.priority}, flow_id: {p.flow_id}")
                self.drop_count = 0

    def size(self):
        return self.q.size()

class CBQ:
    def __init__(self, num_nodes, capacity, bandwidth):
        self.nodes = [Queue(capacity // num_nodes, bandwidth // num_nodes) for _ in range(num_nodes)]

    def enqueue(self, p):
        node_idx = p.flow_id % len(self.nodes)
        if self.nodes[node_idx].enqueue(p):
            print(f"Packet enqueued, size: {p.size}, priority: {p.priority}, flow_id: {p.flow_id}")
        else:
            print(f"Queue full, packet dropped, size: {p.size}")

    def dequeue_all(self):
        for node_idx in range(len(self.nodes)):
            while self.nodes[node_idx].size() > 0:
                p = self.nodes[node_idx].dequeue()
                print(f"Packet dequeued, size: {p.size}, priority: {p.priority}, flow_id: {p.flow_id}")

class FairQ:
    def __init__(self, num_flows, capacity, bandwidth):
        self.flow_queues = [Queue(capacity // num_flows, bandwidth // num_flows) for _ in range(num_flows)]

    def enqueue(self, p):
        flow = p.flow_id % len(self.flow_queues)
        if self.flow_queues[flow].enqueue(p):
            print(f"Packet enqueued, size: {p.size}, priority: {p.priority}, flow_id: {flow}")
        else:
            print(f"Queue full, packet dropped, size: {p.size}")

    def dequeue_all(self):
        for flow in range(len(self.flow_queues)):
            while self.flow_queues[flow].size() > 0:
                p = self.flow_queues[flow].dequeue()
                print(f"Packet dequeued, size: {p.size}, priority: {p.priority}, flow_id: {flow}")

class HFSC:
    def __init__(self, num_nodes, capacity, bandwidth):
        self.nodes = [Queue(capacity // num_nodes, bandwidth // num_nodes) for _ in range(num_nodes)]

    def enqueue(self, p):
        node_idx = p.priority % len(self.nodes)
        if self.nodes[node_idx].enqueue(p):
            print(f"Packet enqueued, size: {p.size}, priority: {p.priority}, flow_id: {p.flow_id}")
        else:
            print(f"Queue full, packet dropped, size: {p.size}")

    def dequeue_all(self):
        for node_idx in range(len(self.nodes)):
            while self.nodes[node_idx].size() > 0:
                p = self.nodes[node_idx].dequeue()
                print(f"Packet dequeued, size: {p.size}, priority: {p.priority}, flow_id: {p.flow_id}")

def simulate_altq(packets, n, capacity, bandwidth):
    print("=== ALTQ Schedulers Simulation ===")
    priq = PRIQ(16, capacity, bandwidth)
    dropped = 0
    print("=== PRIQ Scheduler ===")
    print("Initial queue: empty")

    for p in packets:
        priq.enqueue(p)

    priq.dequeue_all()
    print("Final queue length: 0")
    print("Packets dropped: ", priq.totalDropped())
    print("Final queue: empty")

    # Similar for other schedulers
    print("\n=== CoDel Scheduler ===")
    codel = CoDelQueue(capacity, bandwidth, 5.0, 100.0)
    for p in packets:
        codel.enqueue(p)
    print("Final queue length: ", codel.size())
    print("Packets dropped: 0") # Simplified
    print("Final queue: empty")

    print("\n=== CBQ Scheduler ===")
    cbq = CBQ(4, capacity, bandwidth)
    for p in packets:
        cbq.enqueue(p)
    cbq.dequeue_all()
    print("Final queue length: 0")
    print("Packets dropped: 0")
    print("Final queue: empty")

    print("\n=== FairQ Scheduler ===")
    fairq = FairQ(5, capacity, bandwidth)
    for p in packets:
        fairq.enqueue(p)
    fairq.dequeue_all()
    print("Final queue length: 0")
    print("Packets dropped: 0")
    print("Final queue: empty")

    print("\n=== HFSC Scheduler ===")
    hfsc = HFSC(4, capacity, bandwidth)
    for p in packets:
        hfsc.enqueue(p)
    hfsc.dequeue_all()
    print("Final queue length: 0")
    print("Packets dropped: 0")
    print("Final queue: empty")

if __name__ == "__main__":
    random.seed(42)
    packets = [Packet(random.randint(1, 100), random.randint(1, 16), random.randint(1, 5)) for _ in range(200)]
    simulate_altq(packets, 200, 100, 1000)
