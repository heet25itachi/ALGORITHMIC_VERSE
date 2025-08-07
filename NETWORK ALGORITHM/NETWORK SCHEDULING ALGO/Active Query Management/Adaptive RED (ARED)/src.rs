use rand::Rng;

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
        Queue { items: Vec::with_capacity(capacity), capacity }
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

fn simulate_ared(packets: &[Packet], min_th: f64, max_th: f64, w_q: f64, target: f64, alpha: f64, beta: f64, interval: f64, capacity: usize) {
    let mut q = Queue::new(capacity);
    let mut avg = 0.0;
    let mut max_p = 0.1;
    let mut last_update = 0.0;
    let mut current_time = 0.0;
    let mut count = 0;
    let mut dropped = 0;
    println!("Initial queue: empty");

    for p in packets {
        avg = if q.size() == 0 { 0.0 } else { (1.0 - w_q) * avg + w_q * q.size() as f64 };
        if current_time - last_update >= interval {
            if avg > target && max_p <= 0.5 {
                max_p *= 1.0 + alpha;
            } else if avg < target && max_p >= 0.01 {
                max_p *= beta;
            }
            last_update = current_time;
        }

        let drop = if avg < min_th {
            false
        } else if avg >= max_th {
            true
        } else {
            let pb = max_p * (avg - min_th) / (max_th - min_th);
            let pa = pb / (1.0 - count as f64 * pb);
            count += 1;
            rand::thread_rng().gen::<f64>() < pa
        };

        if drop {
            println!("Packet dropped, size: {}, avg queue length: {:.2}, max_p: {:.4}", p.size, avg, max_p);
            dropped += 1;
        } else if q.enqueue(*p) {
            println!("Packet enqueued, size: {}, avg queue length: {:.2}, max_p: {:.4}", p.size, avg, max_p);
            count = 0;
        } else {
            println!("Queue full, packet dropped, size: {}", p.size);
            dropped += 1;
        }
        current_time += 1.0;
    }

    println!("Final queue length: {}", q.size());
    println!("Packets dropped: {}", dropped);
    q.print_queue();
}

fn simulate_blue(packets: &[Packet], d1: f64, d2: f64, freeze_time: f64, capacity: usize) {
    let mut q = Queue::new(capacity);
    let mut p = 0.0;
    let mut last_update = 0.0;
    let mut current_time = 0.0;
    let mut dropped = 0;
    println!("Initial queue: empty");

    for p in packets {
        if q.size() >= capacity {
            p += d1;
            last_update = current_time;
            println!("Queue full, packet dropped, size: {}, drop prob: {:.4}", p.size, p);
            dropped += 1;
        } else {
            if current_time - last_update >= freeze_time && q.size() == 0 {
                p = if p > d2 { p - d2 } else { 0.0 };
                last_update = current_time;
            }
            if rand::thread_rng().gen::<f64>() < p {
                println!("Packet dropped, size: {}, drop prob: {:.4}", p.size, p);
                dropped += 1;
            } else if q.enqueue(*p) {
                println!("Packet enqueued, size: {}, drop prob: {:.4}", p.size, p);
            }
        }
        current_time += 1.0;
    }

    println!("Final queue length: {}", q.size());
    println!("Packets dropped: {}", dropped);
    q.print_queue();
}

fn simulate_pi(packets: &[Packet], q_ref: f64, a: f64, b: f64, capacity: usize) {
    let mut q = Queue::new(capacity);
    let mut p = 0.0;
    let mut prev_error = 0.0;
    let mut current_time = 0.0;
    let mut dropped = 0;
    println!("Initial queue: empty");

    for p in packets {
        let error = q.size() as f64 - q_ref;
        p += a * error - b * prev_error;
        prev_error = error;
        if p < 0.0 { p = 0.0; } else if p > 1.0 { p = 1.0; }

        if rand::thread_rng().gen::<f64>() < p {
            println!("Packet dropped, size: {}, drop prob: {:.4}", p.size, p);
            dropped += 1;
        } else if q.enqueue(*p) {
            println!("Packet enqueued, size: {}, drop prob: {:.4}", p.size, p);
        } else {
            println!("Queue full, packet dropped, size: {}", p.size);
            dropped += 1;
        }
        current_time += 1.0;
    }

    println!("Final queue length: {}", q.size());
    println!("Packets dropped: {}", dropped);
    q.print_queue();
}

fn main() {
    let mut rng = rand::thread_rng();
    let n = 200;
    let capacity = 100;
    let packets: Vec<Packet> = (0..n).map(|_| Packet { size: rng.gen_range(1..=100), arrival_time: 0.0 }).collect();

    println!("=== ARED ===");
    simulate_ared(&packets, 20.0, 80.0, 0.002, 50.0, 0.01, 0.9, 1000.0, capacity);
    println!("\n=== Blue ===");
    simulate_blue(&packets, 0.0002, 0.00005, 100.0, capacity);
    println!("\n=== PI ===");
    simulate_pi(&packets, 50.0, 0.00001822, 0.00001816, capacity);
}
