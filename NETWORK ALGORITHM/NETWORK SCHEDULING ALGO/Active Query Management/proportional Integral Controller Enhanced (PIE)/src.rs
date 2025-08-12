use rand::distributions::{Distribution, Uniform};
use rand::rngs::StdRng;
use rand::SeedableRng;

// Define Packet and Queue structs
#[derive(Clone)]
struct Packet {
    size: i32,
    arrival_time: f64,
}

struct Queue {
    items: Vec<Packet>,
    capacity: usize,
}

impl Queue {
    fn new(capacity: usize) -> Self {
        Queue { items: Vec::new(), capacity }
    }
    fn enqueue(&mut self, p: Packet) -> bool {
        if self.items.len() >= self.capacity {
            return false;
        }
        self.items.push(p);
        true
    }
    fn dequeue(&mut self) -> Packet {
        self.items.remove(0)
    }
    fn peek(&self) -> &Packet {
        &self.items[0]
    }
    fn size(&self) -> usize {
        self.items.len()
    }
}

// Simulate PIE algorithm
fn simulate_pie(packets: &[Packet], target: f64, update_interval: f64, alpha: f64, beta: f64, max_drop_prob: f64, max_burst: f64, capacity: usize) {
    let mut q = Queue::new(capacity);
    let mut current_time = 0.0;
    let mut last_update = 0.0;
    let mut drop_prob = 0.0;
    let mut prev_delay = 0.0;
    let mut burst_time = max_burst;
    let mut dropped = 0;
    let mut rng = StdRng::seed_from_u64(42);
    let dist = Uniform::new(0.0, 1.0);
    println!("Initial queue: empty");

    for p in packets {
        let mut packet = p.clone();
        packet.arrival_time = current_time;
        let delay = if q.size() == 0 { 0.0 } else { current_time - q.peek().arrival_time };

        if current_time - last_update >= update_interval {
            let error = delay - target;
            drop_prob += alpha * error + beta * (delay - prev_delay);
            drop_prob = drop_prob.max(0.0).min(max_drop_prob);
            prev_delay = delay;
            last_update = current_time;
            if delay > target {
                burst_time = 0.0;
            } else if burst_time < max_burst {
                burst_time += update_interval;
            }
        }

        let drop = burst_time < max_burst && delay > target && dist.sample(&mut rng) < drop_prob;

        if drop {
            println!("Packet dropped, size: {}, queue delay: {:.2}, drop prob: {:.4}", p.size, delay, drop_prob);
            dropped += 1;
        } else if q.enqueue(packet) {
            println!("Packet enqueued, size: {}, queue delay: {:.2}, drop prob: {:.4}", p.size, delay, drop_prob);
            let deq_p = q.dequeue();
            println!("Packet dequeued, size: {}, queue delay: {:.2}", deq_p.size, delay);
        } else {
            println!("Queue full, packet dropped, size: {}", p.size);
            dropped += 1;
        }
        current_time += 1.0;
    }

    while q.size() > 0 {
        let delay = current_time - q.peek().arrival_time;
        if current_time - last_update >= update_interval {
            let error = delay - target;
            drop_prob += alpha * error + beta * (delay - prev_delay);
            drop_prob = drop_prob.max(0.0).min(max_drop_prob);
            prev_delay = delay;
            last_update = current_time;
            if delay > target {
                burst_time = 0.0;
            } else if burst_time < max_burst {
                burst_time += update_interval;
            }
        }
        let deq_p = q.dequeue();
        println!("Packet dequeued, size: {}, queue delay: {:.2}", deq_p.size, delay);
        current_time += 1.0;
    }

    println!("Final queue length: {}", q.size());
    println!("Packets dropped: {}", dropped);
    println!("Final queue: empty");
}

fn main() {
    let mut rng = StdRng::seed_from_u64(42);
    let dist = Uniform::new_inclusive(1, 100);
    let packets: Vec<Packet> = (0..200).map(|_| Packet { size: dist.sample(&mut rng), arrival_time: 0.0 }).collect();
    println!("=== PIE ===");
    simulate_pie(&packets, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100);
}
