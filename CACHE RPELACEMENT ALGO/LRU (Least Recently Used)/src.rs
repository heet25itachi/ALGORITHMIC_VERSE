use std::collections::HashMap;

struct LRUCache {
    capacity: usize,
    items: Vec<(i32, i32)>,
    key_map: HashMap<i32, usize>,
}

impl LRUCache {
    fn new(capacity: usize) -> Self {
        LRUCache {
            capacity,
            items: Vec::new(),
            key_map: HashMap::new(),
        }
    }

    fn get(&mut self, key: i32) -> i32 {
        if let Some(&idx) = self.key_map.get(&key) {
            let value = self.items[idx].1;
            let item = self.items.remove(idx);
            self.items.push(item);
            self.key_map.insert(key, self.items.len() - 1);
            print!("Cache after get({}): [", key);
            for (i, &(k, v)) in self.items.iter().enumerate() {
                print!("({}, {}){}", k, v, if i < self.items.len() - 1 { ", " } else { "" });
            }
            println!("]");
            value
        } else {
            -1
        }
    }

    fn put(&mut self, key: i32, value: i32) {
        if let Some(&idx) = self.key_map.get(&key) {
            self.items.remove(idx);
        } else if self.items.len() == self.capacity {
            let oldest = self.items.remove(0);
            self.key_map.remove(&oldest.0);
        }
        self.items.push((key, value));
        self.key_map.insert(key, self.items.len() - 1);
        print!("Cache after put({}, {}): [", key, value);
        for (i, &(k, v)) in self.items.iter().enumerate() {
            print!("({}, {}){}", k, v, if i < self.items.len() - 1 { ", " } else { "" });
        }
        println!("]");
    }
}

fn main() {
    let mut cache = LRUCache::new(3);
    cache.put(1, 10);
    cache.put(2, 20);
    cache.put(3, 30);
    cache.put(4, 40);
    println!("Get(2) = {}", cache.get(2));
    cache.put(5, 50);
}
