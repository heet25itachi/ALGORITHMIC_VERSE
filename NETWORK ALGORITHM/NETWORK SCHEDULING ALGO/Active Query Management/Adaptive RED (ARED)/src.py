import random

class Packet:
    def __init__(self, size): self.size = size

class Queue:
    def __init__(self, capacity):
        self.items = []
        self.capacity = capacity
    def enqueue(self, p):
        if len(self.items) >= self.capacity: return False
        self.items.append(p)
        return True
    def size(self): return len(self.items)
    def print_queue(self):
        print("Final queue:", " ".join(str(p.size) for p in self.items))

def simulate_ared(packets, min_th, max_th, w_q, target, alpha, beta, interval, capacity):
    q = Queue(capacity)
    avg, max_p, last_update, current_time = 0, 0.1, 0, 0
    count, dropped = 0, 0
    print("Initial queue: empty")

    for p in packets:
        avg = 0 if q.size() == 0 else (1 - w_q) * avg + w_q * q.size()
        if current_time - last_update >= interval:
            if avg > target and max_p <= 0.5: max_p *= (1 + alpha)
            elif avg < target and max_p >= 0.01: max_p *= beta
            last_update = current_time

        drop = False
        if avg < min_th: drop = False
        elif avg >= max_th: drop = True
        else:
            pb = max_p * (avg - min_th) / (max_th - min_th)
            pa = pb / (1 - count * pb)
            drop = random.random() < pa
            count += 1

        if drop:
            print(f"Packet dropped, size: {p.size}, avg queue length: {avg:.2f}, max_p: {max_p:.4f}")
            dropped += 1
        elif q.enqueue(p):
            print(f"Packet enqueued, size: {p.size}, avg queue length: {avg:.2f}, max_p: {max_p:.4f}")
            count = 0
        else:
            print(f"Queue full, packet dropped, size: {p.size}")
            dropped += 1
        current_time += 1.0

    print(f"Final queue length: {q.size()}")
    print(f"Packets dropped: {dropped}")
    q.print_queue()

def simulate_blue(packets, d1, d2, freeze_time, capacity):
    q = Queue(capacity)
    p, last_update, current_time = 0, 0, 0
    dropped = 0
    print("Initial queue: empty")

    for p in packets:
        if q.size() >= capacity:
            p += d1
            last_update = current_time
            print(f"Queue full, packet dropped, size: {p.size}, drop prob: {p:.4f}")
            dropped += 1
        else:
            if current_time - last_update >= freeze_time and q.size() == 0:
                p = max(p - d2, 0)
                last_update = current_time
            if random.random() < p:
                print(f"Packet dropped, size: {p.size}, drop prob: {p:.4f}")
                dropped += 1
            elif q.enqueue(p):
                print(f"Packet enqueued, size: {p.size}, drop prob: {p:.4f}")
        current_time += 1.0

    print(f"Final queue length: {q.size()}")
    print(f"Packets dropped: {dropped}")
    q.print_queue()

def simulate_pi(packets, q_ref, a, b, capacity):
    q = Queue(capacity)
    p, prev_error, current_time = 0, 0, 0
    dropped = 0
    print("Initial queue: empty")

    for p in packets:
        error = q.size() - q_ref
        p += a * error - b * prev_error
        prev_error = error
        if p < 0: p = 0
        elif p > 1: p = 1

        if random.random() < p:
            print(f"Packet dropped, size: {p.size}, drop prob: {p:.4f}")
            dropped += 1
        elif q.enqueue(p):
            print(f"Packet enqueued, size: {p.size}, drop prob: {p:.4f}")
        else:
            print(f"Queue full, packet dropped, size: {p.size}")
            dropped += 1
        current_time += 1.0

    print(f"Final queue length: {q.size()}")
    print(f"Packets dropped: {dropped}")
    q.print_queue()

if __name__ == "__main__":
    random.seed()
    n, capacity = 200, 100
    packets = [Packet(random.randint(1, 100)) for _ in range(n)]

    print("=== ARED ===")
    simulate_ared(packets, 20, 80, 0.002, 50, 0.01, 0.9, 1000, capacity)
    print("\n=== Blue ===")
    simulate_blue(packets, 0.0002, 0.00005, 100, capacity)
    print("\n=== PI ===")
    simulate_pi(packets, 50, 0.00001822, 0.00001816, capacity)
