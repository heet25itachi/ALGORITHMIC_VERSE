struct FIFOCache {
    capacity: usize,
    items: Vec<(i32, i32)>,
}

impl FIFOCache {
    fn new(capacity: usize) -> Self {
        FIFOCache { capacity, items: Vec::new() }
    }

    fn get(&self, key: i32) -> i32 {
        for &(k, v) in &self.items {
            if k == key {
                print!("Cache after get({}): [", key);
                for (i, &(k, v)) in self.items.iter().enumerate() {
                    print!("({}, {}){}", k, v, if i < self.items.len() - 1 { ", " } else { "" });
                }
                println!("]");
                return v;
            }
        }
        -1
    }

    fn put(&mut self, key: i32, value: i32) {
        for i in 0..self.items.len() {
            if self.items[i].0 == key {
                self.items[i].1 = value;
                print!("Cache after put({}, {}): [", key, value);
                for (i, &(k, v)) in self.items.iter().enumerate() {
                    print!("({}, {}){}", k, v, if i < self.items.len() - 1 { ", " } else { "" });
                }
                println!("]");
                return;
            }
        }
        if self.items.len() == self.capacity {
            self.items.remove(0);
        }
        self.items.push((key, value));
        print!("Cache after put({}, {}): [", key, value);
        for (i, &(k, v)) in self.items.iter().enumerate() {
            print!("({}, {}){}", k, v, if i < self.items.len() - 1 { ", " } else { "" });
        }
        println!("]");
    }
}

fn main() {
    let mut cache = FIFOCache::new(3);
    cache.put(1, 10);
    cache.put(2, 20);
    cache.put(3, 30);
    cache.put(4, 40);
    println!("Get(2) = {}", cache.get(2));
    cache.put(5, 50);
}
