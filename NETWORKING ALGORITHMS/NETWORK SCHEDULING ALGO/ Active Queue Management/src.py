import random

class Packet:
    def __init__(self, size):
        self.size = size

class REDQueue:
    def __init__(self, capacity):
        self.items = []
        self.capacity = capacity
        self.avg = 0
        self.count = 0

    def enqueue(self, packet, min_th, max_th, w_q, max_p):
        if not self.items:
            self.avg = 0
        else:
            self.avg = (1 - w_q) * self.avg + w_q * len(self.items)

        drop = False
        if self.avg < min_th:
            drop = False
        elif self.avg >= max_th:
            drop = True
        else:
            pb = max_p * (self.avg - min_th) / (max_th - min_th)
            pa = pb / (1 - self.count * pb)
            if random.random() < pa:
                drop = True
            else:
                drop = False
            self.count += 1

        if drop:
            print(f"Packet dropped, size: {packet.size}, avg queue length: {self.avg:.2f}")
            return False
        elif len(self.items) < self.capacity:
            self.items.append(packet.size)
            print(f"Packet enqueued, size: {packet.size}, avg queue length: {self.avg:.2f}")
            self.count = 0
            return True
        else:
            print(f"Queue full, packet dropped, size: {packet.size}")
            return False

    def size(self):
        return len(self.items)

    def print_queue(self):
        print("Final queue:", " ".join(map(str, self.items)))

def simulate_red(packets, min_th, max_th, w_q, max_p, capacity):
    q = REDQueue(capacity)
    dropped = 0
    print("Initial queue: empty")

    for p in packets:
        if not q.enqueue(p, min_th, max_th, w_q, max_p):
            dropped += 1

    print(f"Final queue length: {q.size()}")
    print(f"Packets dropped: {dropped}")
    q.print_queue()

if __name__ == "__main__":
    random.seed()
    n, capacity = 200, 100
    min_th, max_th, w_q, max_p = 20, 80, 0.002, 0.1
    packets = [Packet(random.randint(1, 100)) for _ in range(n)]

    simulate_red(packets, min_th, max_th, w_q, max_p, capacity)
