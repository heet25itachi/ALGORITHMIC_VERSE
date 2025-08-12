import random

# Define Packet and Queue classes
class Packet:
    def __init__(self, size):
        self.size = size

class Queue:
    def __init__(self, capacity):
        self.items = []
        self.capacity = capacity
    def enqueue(self, packet):
        if len(self.items) >= self.capacity:
            return False
        self.items.append(packet)
        return True
    def size(self):
        return len(self.items)
    def print_queue(self):
        print("Final queue:", " ".join(str(p.size) for p in self.items))

# Simulate RED algorithm
def simulate_red(packets, min_th, max_th, w_q, max_p, capacity):
    q = Queue(capacity)
    avg, count, dropped = 0, 0, 0
    print("Initial queue: empty")
    for p in packets:
        avg = 0 if q.size() == 0 else (1 - w_q) * avg + w_q * q.size()
        drop = False
        if avg < min_th:
            drop = False
        elif avg >= max_th:
            drop = True
        else:
            pb = max_p * (avg - min_th) / (max_th - min_th)
            pa = pb / (1 - count * pb)
            count += 1
            drop = random.random() < pa
        if drop:
            print(f"Packet dropped, size: {p.size}, avg queue length: {avg:.2f}, max_p: {max_p:.4f}")
            dropped += 1
        elif q.enqueue(p):
            print(f"Packet enqueued, size: {p.size}, avg queue length: {avg:.2f}, max_p: {max_p:.4f}")
            count = 0
        else:
            print(f"Queue full, packet dropped, size: {p.size}")
            dropped += 1
    print(f"Final queue length: {q.size()}")
    print(f"Packets dropped: {dropped}")
    q.print_queue()

# Main
if __name__ == "__main__":
    random.seed(42)
    packets = [Packet(random.randint(1, 100)) for _ in range(200)]
    print("=== RED ===")
    simulate_red(packets, 20, 80, 0.002, 0.1, 100)
