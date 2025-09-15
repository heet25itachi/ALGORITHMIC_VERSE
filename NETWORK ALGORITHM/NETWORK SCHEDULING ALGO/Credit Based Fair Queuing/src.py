import random
from collections import deque

class Packet:
    def __init__(self, size, weight, flow_id):
        self.size = size
        self.weight = weight
        self.flow_id = flow_id

class Queue:
    def __init__(self, capacity, credit_rate, total_bandwidth):
        self.items = deque()
        self.capacity = capacity
        self.credit = 0.0
        self.credit_rate = credit_rate
        self.total_bandwidth = total_bandwidth

    def enqueue(self, p):
        if len(self.items) >= self.capacity:
            return False
        self.items.append(p)
        return True

    def dequeue(self):
        return self.items.popleft()

    def update_credit(self, delta_time):
        self.credit += self.credit_rate * delta_time
        if self.credit < 0:
            self.credit = 0.0

    def size(self):
        return len(self.items)

def simulate_cbfq(packets, capacity, bandwidth, base_rate):
    queues = [Queue(capacity // 5, weight * base_rate, bandwidth // 5) for weight in [1.0, 2.0, 3.0, 4.0, 5.0]]
    current_time = 0.0
    service_rate = 1000.0
    dropped = 0
    print("=== CBFQ Scheduler ===")
    print("Initial queue: empty")

    for p in packets:
        queue_idx = p.flow_id % 5
        queues[queue_idx].update_credit(0.001)

        if queues[queue_idx].enqueue(p):
            print(f"Packet enqueued, size: {p.size}, weight: {int(queue_idx + 1)}, flow_id: {p.flow_id}, credit: {queues[queue_idx].credit:.2f}")
        else:
            print(f"Queue full, packet dropped, size: {p.size}")
            dropped += 1

        max_credit = -1.0
        max_idx = -1
        for j in range(5):
            if queues[j].size() > 0 and queues[j].credit > max_credit:
                max_credit = queues[j].credit
                max_idx = j
        if max_idx != -1:
            p = queues[max_idx].dequeue()
            queues[max_idx].credit -= service_rate * 0.001
            if queues[max_idx].credit < 0:
                queues[max_idx].credit = 0.0
            print(f"Packet dequeued, size: {p.size}, weight: {int(max_idx + 1)}, flow_id: {p.flow_id}, credit: {queues[max_idx].credit:.2f}")
        current_time += 0.001

    for queue_idx in range(5):
        while queues[queue_idx].size() > 0:
            p = queues[queue_idx].dequeue()
            queues[queue_idx].credit -= service_rate * 0.001
            if queues[queue_idx].credit < 0:
                queues[queue_idx].credit = 0.0
            print(f"Packet dequeued, size: {p.size}, weight: {int(queue_idx + 1)}, flow_id: {p.flow_id}, credit: {queues[queue_idx].credit:.2f}")

    print("Final queue length: 0")
    print(f"Packets dropped: {dropped}")
    print("Final queue: empty")

if __name__ == "__main__":
    random.seed(42)
    packets = [Packet(random.randint(1, 100), random.randint(1, 5), random.randint(0, 4)) for _ in range(200)]
    simulate_cbfq(packets, 100, 1000, 1.0)
