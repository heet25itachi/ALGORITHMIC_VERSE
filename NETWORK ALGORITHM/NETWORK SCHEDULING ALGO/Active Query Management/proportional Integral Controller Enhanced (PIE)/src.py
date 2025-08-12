import random

# Define Packet and Queue classes
class Packet:
    def __init__(self, size):
        self.size = size
        self.arrival_time = 0.0

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
    def peek(self):
        return self.items[0]
    def size(self):
        return len(self.items)

# Simulate PIE algorithm
def simulate_pie(packets, target, update_interval, alpha, beta, max_drop_prob, max_burst, capacity):
    q = Queue(capacity)
    current_time, last_update, drop_prob, prev_delay, burst_time = 0.0, 0.0, 0.0, 0.0, max_burst
    dropped = 0
    print("Initial queue: empty")
    random.seed(42)

    for p in packets:
        p.arrival_time = current_time
        delay = 0.0 if q.size() == 0 else current_time - q.peek().arrival_time

        if current_time - last_update >= update_interval:
            error = delay - target
            drop_prob += alpha * error + beta * (delay - prev_delay)
            drop_prob = max(0, min(max_drop_prob, drop_prob))
            prev_delay = delay
            last_update = current_time
            if delay > target:
                burst_time = 0
            elif burst_time < max_burst:
                burst_time += update_interval

        drop = burst_time < max_burst and delay > target and random.random() < drop_prob

        if drop:
            print(f"Packet dropped, size: {p.size}, queue delay: {delay:.2f}, drop prob: {drop_prob:.4f}")
            dropped += 1
        elif q.enqueue(p):
            print(f"Packet enqueued, size: {p.size}, queue delay: {delay:.2f}, drop prob: {drop_prob:.4f}")
            deq_p = q.dequeue()
            print(f"Packet dequeued, size: {deq_p.size}, queue delay: {delay:.2f}")
        else:
            print(f"Queue full, packet dropped, size: {p.size}")
            dropped += 1
        current_time += 1.0

    while q.size() > 0:
        delay = current_time - q.peek().arrival_time
        if current_time - last_update >= update_interval:
            error = delay - target
            drop_prob += alpha * error + beta * (delay - prev_delay)
            drop_prob = max(0, min(max_drop_prob, drop_prob))
            prev_delay = delay
            last_update = current_time
            if delay > target:
                burst_time = 0
            elif burst_time < max_burst:
                burst_time += update_interval
        deq_p = q.dequeue()
        print(f"Packet dequeued, size: {deq_p.size}, queue delay: {delay:.2f}")
        current_time += 1.0

    print(f"Final queue length: {q.size()}")
    print(f"Packets dropped: {dropped}")
    print("Final queue: empty")

# Main
if __name__ == "__main__":
    packets = [Packet(random.randint(1, 100)) for _ in range(200)]
    print("=== PIE ===")
    simulate_pie(packets, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100)
