import random

class Packet:
    def __init__(self, size, arrival_time=0.0):
        self.size = size
        self.arrival_time = arrival_time

class CoDelQueue:
    def __init__(self, capacity):
        self.items = []
        self.capacity = capacity
        self.first_above_time = 0.0
        self.drop_next = float('inf')
        self.drop_count = 0

    def enqueue(self, p):
        if len(self.items) >= self.capacity:
            print(f"Queue full, packet dropped, size: {p.size}")
            return False
        self.items.append(p)
        print(f"Packet enqueued, size: {p.size}")
        return True

    def dequeue(self, current_time, target, interval):
        if not self.items:
            return None
        p = self.items[0]
        sojourn_time = current_time - p.arrival_time

        if sojourn_time < target or len(self.items) <= 4:
            self.first_above_time = 0.0
            self.drop_next = float('inf')
            self.items.pop(0)
            print(f"Packet dequeued, size: {p.size}, sojourn time: {sojourn_time:.2f}")
            self.drop_count = 0
            return p
        elif self.first_above_time == 0.0
            self.first_above_time = current_time + interval
            self.drop_next = self.first_above_time
            self.items.pop(0)
            print(f"Packet dequeued, size: {p.size}, sojourn time: {sojourn_time:.2f}")
            return p
        elif current_time >= self.drop_next
            self.items.pop(0)
            print(f"Packet dropped, size: {p.size}, sojourn time: {sojourn_time:.2f}")
            self.drop_count += 1
            self.drop_next = current_time + interval / math.sqrt(self.drop_count)
            return None
        else
            self.items.pop(0)
            print(f"Packet dequeued, size: {p.size}, sojourn time: {sojourn_time:.2f}")
            self.drop_count = 0
            return p
    end

    def size(self):
        return len(self.items)
end

def simulate_codel(packets, target, interval, capacity):
    q = CoDelQueue(capacity)
    current_time = 0.0
    dropped = 0
    print("Initial queue: empty")

    for p in packets:
        p.arrival_time = current_time
        if q.enqueue(p):
            while q.size() > 0:
                result = q.dequeue(current_time, target, interval)
                if result is None:
                    dropped += 1
        else:
            dropped += 1
        current_time += 1.0

    while q.size() > 0:
        result = q.dequeue(current_time, target, interval)
        if result is None:
            dropped += 1
        current_time += 1.0

    print("Final queue length: ", q.size())
    print("Packets dropped: ", dropped)
    print("Final queue: empty")

packets = [Packet(random.randint(1, 100)) for _ in range(200)]
simulate_codel(packets, 5, 100, 100)
