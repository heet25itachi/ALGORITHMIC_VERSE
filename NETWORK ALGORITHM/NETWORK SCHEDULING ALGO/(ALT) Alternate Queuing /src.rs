use rand::Rng;
use std::collections::VecDeque;

#[derive(Clone, Copy)]
struct Packet {
    size: i32,
    priority: i32,
    flow_id: i32,
}

struct Queue {
    items: VecDeque<Packet>,
    capacity: usize,
    total_bandwidth: i32,
}

impl Queue {
    fn new(capacity: usize, total_bandwidth: i32) -> Self {
        Queue {
            items: VecDeque::new(),
            capacity,
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

    fn dequeue(&mut self) -> Option<Packet> {
        self.items.pop_front()
    }

    fn size(&self) -> usize {
        self.items.len()
    }
}

struct PRIQ {
    queues: Vec<Queue>,
}

impl PRIQ {
    fn new(num_queues: usize, capacity: usize, bandwidth: i32) -> Self {
        let queues = (0..num_queues)
            .map(|_| Queue::new(capacity / num_queues, bandwidth / num_queues as i32))
            .collect();
        PRIQ { queues }
    }

    fn enqueue(&mut self, p: Packet) {
        let pri = (p.priority % self.queues.len() as i32) as usize;
        if self.queues[pri].enqueue(p) {
            println!("Packet enqueued, size: {}, priority: {}, flow_id: {}", p.size, pri, p.flow_id);
        } else {
            println!("Queue full, packet dropped, size: {}", p.size);
        }
    }

    fn dequeue_all(&mut self) {
        for pri in (0..self.queues.len()).rev() {
            while let Some(p) = self.queues[pri].dequeue() {
                println!("Packet dequeued, size: {}, priority: {}, flow_id: {}", p.size, pri, p.flow_id);
            }
        }
    }
}

struct CoDelQueue {
    q: Queue,
    target_delay: f64,
    interval: f64,
    first_above_time: f64,
    drop_next: f64,
    drop_count: i32,
}

impl CoDelQueue {
    fn new(capacity: usize, bandwidth: i32, target: f64, interval: f64) -> Self {
        CoDelQueue {
            q: Queue::new(capacity, bandwidth),
            target_delay: target,
            interval,
            first_above_time: 0.0,
            drop_next: f64::INFINITY,
            drop_count: 0,
        }
    }

    fn enqueue(&mut self, p: Packet, current_time: f64) -> bool {
        if self.q.enqueue(p) {
            println!("Packet enqueued, size: {}, priority: {}, flow_id: {}", p.size, p.priority, p.flow_id);
            self.process_queue(current_time);
            true
        } else {
            println!("Queue full, packet dropped, size: {}", p.size);
            false
        }
    }

    fn process_queue(&mut self, current_time: f64) {
        while self.q.size() > 0 {
            let sojourn_time = current_time; // Simplified
            if sojourn_time < self.target_delay || self.q.size() <= 4 {
                if let Some(p) = self.q.dequeue() {
                    println!("Packet dequeued, size: {}, priority: {}, flow_id: {}", p.size, p.priority, p.flow_id);
                }
                self.first_above_time = 0.0;
                self.drop_next = f64::INFINITY;
                self.drop_count = 0;
            } else if self.first_above_time == 0.0 {
                self.first_above_time = current_time + self.interval;
                self.drop_next = self.first_above_time;
                if let Some(p) = self.q.dequeue() {
                    println!("Packet dequeued, size: {}, priority: {}, flow_id: {}", p.size, p.priority, p.flow_id);
                }
            } else if current_time >= self.drop_next {
                if let Some(p) = self.q.dequeue() {
                    println!("Packet dropped, size: {}, priority: {}, flow_id: {}", p.size, p.priority, p.flow_id);
                }
                self.drop_count += 1;
                self.drop_next = current_time + self.interval / (self.drop_count as f64).sqrt();
            } else {
                if let Some(p) = self.q.dequeue() {
                    println!("Packet dequeued, size: {}, priority: {}, flow_id: {}", p.size, p.priority, p.flow_id);
                }
                self.drop_count = 0;
            }
        }
    }

    fn size(&self) -> usize {
        self.q.size()
    }
}

struct CBQ {
    nodes: Vec<Queue>,
}

impl CBQ {
    fn new(num_nodes: usize, capacity: usize, bandwidth: i32) -> Self {
        let nodes = (0..num_nodes)
            .map(|_| Queue::new(capacity / num_nodes, bandwidth / num_nodes as i32))
            .collect();
        CBQ { nodes }
    }

    fn enqueue(&mut self, p: Packet) {
        let node_idx = (p.flow_id % self.nodes.len() as i32) as usize;
        if self.nodes[node_idx].enqueue(p) {
            println!("Packet enqueued, size: {}, priority: {}, flow_id: {}", p.size, p.priority, p.flow_id);
        } else {
            println!("Queue full, packet dropped, size: {}", p.size);
        }
    }

    fn dequeue_all(&mut self) {
        for node in &mut self.nodes {
            while let Some(p) = node.dequeue “

System: * Today's date and time is 10:12 AM IST on Monday, September 15, 2025.
