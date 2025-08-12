use rand::Rng;
use std::collections::VecDeque;

#[derive(Clone)]
struct Packet {
    size: i32,
    arrival_time: f64,
}

struct CoDelQueue {
    items: VecDeque<Packet>,
    capacity: usize,
    first_above_time: f64,
    drop_next: f64,
    drop_count: i32,
}

impl CoDelQueue {
    fn new(capacity: usize) -> Self {
        CoDelQueue {
            items: VecDeque::new(),
            capacity,
            first_above_time: 0.0,
            drop_next: f64::INFINITY,
            drop_count: 0,
        }
    }

    fn enqueue(&mut self, p: Packet) -> bool {
        if self.items.len() >= self.capacity {
            println!("Queue full, packet dropped, size: {}", p.size);
            false
        } else {
            self.items.push_back(p);
            println!("Packet enqueued, size: {}", p.size);
            true
        }
    }

    fn dequeue(&mut self, current_time: f64, target: f64, interval: f64) {
        while !self.items.is_empty() {
            let mut p = self.items[0].clone();
            let sojourn_time = current_time - p.arrival_time;

            if sojourn_time < target || self.items.len() <= 4 {
                self.first_above_time = 0.0;
                self.drop_next = f64::INFINITY;
                self.items.pop_front();
                println!("Packet dequeued, size: {}, sojourn time: {:.2}", p.size, sojourn_time);
                self.drop_count = 0;
            } else if self.first_above_time == 0.0 {
                self.first_above_time = current_time + interval;
                self.drop_next = self.first_above_time;
                self.items.pop_front();
                println!("Packet dequeued, size: {}, sojourn time: {:.2}", p.size, sojourn_time);
            } else if current_time >= self.drop_next {
                self.items.pop_front();
                println!("Packet dropped, size: {}, sojourn time: {:.2}", p.size, sojourn_time);
                self.drop_count += 1;
                self.drop_next = current_time + interval / f64::sqrt(self.drop_count as f64);
            } else {
                self.items.pop_front();
                println!("Packet dequeued, size: {}, sojourn time: {:.2}", p.size, sojourn_time);
                self.drop_count = 0;
            }
        }
    }
}

fn simulate_codel(packets: &[Packet], target: f64, interval: f64, capacity: usize) {
    let mut q = CoDelQueue::new(capacity);
    let mut current_time = 0.0;
    let mut dropped = 0;
    println!("Initial queue: empty");

    for p in packets {
        let mut packet = p.clone();
        packet.arrival_time = current_time;
        if q.enqueue(packet) {
            q.dequeue(current_time, target, interval);
        } else {
            dropped += 1;
        }
        current_time += 1.0;
    }

    q.dequeue(current_time, target, interval);

    println!("Final queue length: {}", q.items.len());
    println!("Packets dropped: {}", dropped);
    println!("Final queue: empty");
}

fn main() {
    let mut rng = rand::thread_rng();
    let n = 200;
    let packets: Vec<Packet> = (0..n).map(|_| Packet { size: rng.gen_range(1..=100), arrival_time: 0.0 }).collect();

    simulate_codel(&packets, 5.0, 100.0, 100);
}
