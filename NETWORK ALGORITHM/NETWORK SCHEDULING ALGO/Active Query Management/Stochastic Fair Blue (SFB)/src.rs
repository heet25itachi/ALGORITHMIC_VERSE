use rand::Rng;

#[derive(Clone)]
struct Packet {
    size: i32,
    flow_id: i32,
}

struct Bin {
    p: f64,
    last_update: f64,
}

struct SFB {
    bins: Vec<Bin>,
    l: usize,
    n: usize,
}

impl SFB {
    fn new(l: usize, n: usize) -> Self {
        SFB {
            bins: vec![Bin { p: 0.0, last_update: 0.0 }; l * n],
            l,
            n,
        }
    }

    fn drop(&mut self, flow_id: i32, current_time: f64, d1: f64, d2: f64, freeze_time: f64, queue_length: usize, capacity: usize) -> bool {
        let mut marked = true
        for l in 0..self.l {
            let bin_idx = l * self.n + ((flow_id + l as i32) % self.n as i32) as usize; // Simple hash
            let bin = &mut self.bins[bin_idx];
            if current_time - bin.last_update >= freeze_time {
                if queue_length == 0 {
                    bin.p = bin.p - d2;
                    if bin.p < 0.0 { bin.p = 0.0; }
                } else if queue_length >= capacity {
                    bin.p += d1;
                }
                bin.last_update = current_time;
            }
            if bin.p < 1.0 {
                marked = false;
                break;
            }
        }
        marked
    }
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

    fn size(&self) -> usize {
        self.items.len()
    }
}

fn simulate_sfb(packets: &[Packet], d1: f64, d2: f64, freeze_time: f64, capacity: usize, l: usize, n: usize) {
    let mut q = Queue::new(capacity);
    let mut sfb = SFB::new(l, n);
    let mut current_time = 0.0;
    let mut dropped = 0;
    println!("Initial queue: empty");

    let mut rng = rand::thread_rng();
    for p in packets {
        if sfb.drop(p.flow_id, current_time, d1, d2, freeze_time, q.size(), capacity) {
            println!("Packet dropped, size: {}, flow_id: {}", p.size, p.flow_id);
            dropped += 1;
        } else if q.enqueue(p.clone()) {
            println!("Packet enqueued, size: {}, flow_id: {}", p.size, p.flow_id);
            // Immediate dequeue for simulation
            let deq_p = q.dequeue();
            println!("Packet dequeued, size: {}, flow_id: {}", deq_p.size, deq_p.flow_id);
        } else {
            println!("Queue full, packet dropped, size: {}", p.size);
            dropped += 1;
        }
        current_time += 1.0;
    }

    while q.size() > 0 {
        let p = q.dequeue();
        println!("Packet dequeued, size: {}, flow_id: {}", p.size, p.flow_id);
        current_time += 1.0;
    }

    println!("Final queue length: {}", q.size());
    println!("Packets dropped: {}", dropped);
    println!("Final queue: empty");
}

fn main() {
    let mut rng = rand::thread_rng();
    let packets: Vec<Packet> = (0..200).map(|_| Packet { size: rng.gen_range(1..=100), flow_id: rng.gen_range(1..=20) }).collect();

    println!("=== SFB ===");
    simulate_sfb(&packets, 0.0002, 0.00005, 100.0, 100, 2, 4);
}
