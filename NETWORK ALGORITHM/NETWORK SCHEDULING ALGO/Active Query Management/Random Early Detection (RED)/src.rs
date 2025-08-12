use rand::distributions::{Distribution, Uniform};
use rand::rngs::StdRng;
use rand::SeedableRng;

// Define Packet and Queue structs
#[derive(Clone)]
struct Packet {
    size: i32,
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
    fn size(&self) -> usize {
        self.items.len()
    }
    fn print_queue(&self) {
        print!("Final queue: ");
        for p in &self.items {
            print!("{} ", p.size);
        }
        println!();
    }
}

// Simulate RED algorithm
fn simulate_red(packets: &[Packet], min_th: f64, max_th: f64, w_q: f64, max_p: f64, capacity: usize) {
    let mut q = Queue::new(capacity);
    let mut avg = 0.0;
    let mut count = 0.0;
    let mut dropped = 0.0;
    let mut rng = StdRng::seed_from_u64(42);
    let dist = Uniform::new(0.0, 1.0);
    println!("Initial queue: empty");
    for p in packets {
        avg = if q.size() == 0 { 0.0 } else { (1.0 - w_q) * avg + w_q * q.size() as f64 };
        let drop = if avg < min_th {
            false
        } else if avg >= max_th {
            true
        } else {
            let pb = max_p * (avg - min_th) / (max_th - min_th);
            let pa = pb / (1.0 - count * pb);
            count += 1.0;
            dist.sample(&mut rng) < pa
        };
        if drop {
            println!("Packet dropped, size: {}, avg queue length: {:.2}, max_p: {:.4}", p.size, avg, max_p);
            dropped += 1.0;
        } else if q.enqueue(p.clone()) {
            println!("Packet enqueued, size: {}, avg queue length: {:.2}, max_p: {:.4}", p.size, avg, max_p);
            count = 0.0;
        } else {
            println!("Queue full, packet dropped, size: {}", p.size);
            dropped += 1.0;
        }
    }
    println!("Final queue length: {}", q.size());
    println!("Packets dropped: {}", dropped as i32);
    q.print_queue();
}

fn main() {
    let mut rng = StdRng::seed_from_u64(42);
    let dist = Uniform::new_inclusive(1, 100);
    let packets: Vec<Packet> = (0..200).map(|_| Packet { size: dist.sample(&mut rng) }).collect();
    println!("=== RED ===");
    simulate_red(&packets, 20.0, 80.0, 0.002, 0.1, 100);
}
