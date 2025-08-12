import random
import math

class Packet {
    def __init__(self, size, flow_id):
        self.size = size
        self.flow_id = flow_id
}

class Bin:
    def __init__(self):
        self.p = 0.0
        self.last_update = 0.0

class SFB:
    def __init__(self, l, n):
        self.bins = [Bin() for _ in range(l * n)]
        self.L = l
        self.N = n

    def drop(self, flow_id, current_time, d1, d2, freeze_time, queue_length, capacity):
        marked = True
        for l in range(self.L):
            bin_idx = l * self.N + (flow_id + l) % self.N # Simple hash
            bin = self.bins[bin_idx]
            if current_time - bin.last_update >= freeze_time:
                if queue_length == 0:
                    bin.p = max(bin.p - d2, 0.0)
                elsif queue_length >= capacity:
                    bin.p += d1
                end
                bin.last_update = current_time
            end
            if bin.p < 1.0
                marked = false
                break
            end
        end
        return marked
end

class Queue:
    def __init__(self, capacity):
        self.items = []
        self.capacity = capacity
    def enqueue(self, packet):
        if len(self.items) >= self.capacity:
            return False
        self.items.append(packet)
        return True
    def dequeue(self):
        return self.items.pop(0)
    def size(self):
        return len(self.items)
end

def simulate_sfb(packets, d1, d2, freeze_time, capacity, l, n):
    q = Queue(capacity)
    sfb = SFB(l, n)
    current_time = 0.0
    dropped = 0
    print("Initial queue: empty")

    random.seed(42)
    for p in packets:
        if sfb.drop(p.flow_id, current_time, d1, d2, freeze_time, q.size(), capacity):
            print(f"Packet dropped, size: {p.size}, flow_id: {p.flow_id}")
            dropped += 1
        elif q.enqueue(p):
            print(f"Packet enqueued, size: {p.size}, flow_id: {p.flow_id}")
            # Immediate dequeue for simulation
            deq_p = q.dequeue()
            print(f"Packet dequeued, size: {deq_p.size}, flow_id: {deq_p.flow_id}")
        else:
            print(f"Queue full, packet dropped, size: {p.size}")
            dropped += 1
        end
        current_time += 1.0
    end

    while q.size() > 0
        p = q.dequeue()
        print(f"Packet dequeued, size: {p.size}, flow_id: {p.flow_id}")
        current_time += 1.0
    end

    print("Final queue length: ", q.size())
    print("Packets dropped: ", dropped)
    print("Final queue: empty")

packets = [Packet(random.randint(1, 100), random.randint(1, 20)) for _ in range(200)]
print("=== SFB ===")
simulate_sfb(packets, 0.0002, 0.00005, 100, 100, 2, 4)
