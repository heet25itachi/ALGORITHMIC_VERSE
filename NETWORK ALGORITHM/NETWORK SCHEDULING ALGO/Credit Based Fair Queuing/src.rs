use rand::Rng;
use std::collections::VecDeque;

#[derive(Clone, Copy)]
struct Packet {
    size: i32,
    weight: i32,
    flow_id: i32,
}

struct Queue {
    items: VecDeque<Packet>,
    capacity: usize,
    credit: f64,
    credit_rate: f64,
    total_bandwidth: i32,
}

impl Queue {
    fn new(capacity: usize, credit_rate: f64, total_bandwidth: i32) -> Self {
        Queue {
            items: VecDeque::new(),
            capacity,
            credit: 0.0,
            credit_rate,
            total_bandwidth,
        }
    }

    fn enqueue(&mut self, p: Packet) -> bool {
        if self.items.len() >= self.capacity {
            return false;
        }
        self.items.push_back(p);
        true
    }

    fn dequeue(&mut self) -> Packet {
        self.items.pop_front().unwrap()
    }

    fn update_credit(&mut self, delta_time: f64) {
        self.credit += self.credit_rate * delta_time;
        if self.credit < 0.0 {
            self.credit = 0.0;
        }
    }

    fn spend_credit(&mut self, amount: f64) {
        self.credit -= amount;
        if self.credit < 0.0 {
            self.credit = 0.0;
        }
    }

    fn size(&self) -> usize {
        self.items.len()
    }
}

fn simulate_cbfq(packets: Vec<Packet>, capacity: usize, bandwidth: i32, base_rate: f64) {
    let weights = vec![1.0, 2.0, 3.0, 4.0, 5.0];
    let mut queues: Vec<Queue> = weights
        .iter()
        .map(|&w| Queue::new(capacity / 5, w * base_rate, bandwidth / 5))
        .collect();
    let mut current_time = 0.0;
    let service_rate = 1000.0;
    let mut dropped = 0;
    println!("=== CBFQ Scheduler ===");
    println!("Initial queue: empty");

    for p in packets {
        let queue_idx = (p.flow_id % 5) as usize;
        queues[queue_idx].update_credit(0.001);

        if queues[queue_idx].enqueue(p) {
            println!(
                "Packet enqueued, size: {}, weight: {}, flow_id: {}, credit: {:.2}",
                p.size, weights[queue_idx], p.flow_id, queues[queue_idx].credit
            );
        } else {
            println!("Queue full, packet dropped, size: {}", p.size);
            dropped += 1;
        }

        let mut max_credit = -1.0;
        let mut max_idx = None;
        for (j, q) in queues.iter().enumerate() {
            if q.size() > 0 && q.credit > max_credit {
                max_credit = q.credit;
                max_idx = Some(j);
            }
        }
        if let Some(max_idx) = max_idx {
            let p = queues[max_idx].dequeue();
            queues[max_idx].spend_credit(service_rate * 0.001);
            println!(
                "Packet dequeued, size: {}, weight: {}, flow_id: {}, credit: {:.2}",
                p.size, weights[max_idx], p.flow_id, queues[max_idx].credit
            );
        }
        current_time += 0.001;
    }

    for (queue_idx, q) in queues.iter_mut().enumerate() {
        while q.size() > 0 {
            let p = q.dequeue();
            q.spend_credit(service_rate * 0.001);
            println!(
                "Packet dequeued, size: {}, weight: {}, flow_id: {}, credit: {:.2}",
                p.size, weights[queue_idx], p.flow_id, q.credit
            );
        }
    }

    println!("Final queue length: 0");
    println!("Packets dropped: {}", dropped);
    println!("Final queue: empty");
}

fn main() {
    let mut rng = rand::thread_rng();
    let packets: Vec<Packet> = (0..200)
        .map(|_| Packet {
            size: rng.gen_range(1..=100),
            weight: rng.gen_range(1..=5),
            flow_id: rng.gen_range(0..5),
        })
        .collect();
    simulate_cbfq(packets, 100, 1000, 1.0);
}
