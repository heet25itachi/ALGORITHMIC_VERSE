use std::collections::HashMap;

struct MRUCache {
    capacity: usize,
    items: Vec<(i32, i32)>,
    key_map: HashMap<i32, usize>,
}

impl MRUCache {
    fn new(capacity: usize) -> Self {
        MRUCache {
            capacity,
            items: Vec::new(),
            key_map: HashMap::new(),
        }
    }

    fn get(&mut self, key: i32) -> i32 {
        if let Some(&idx) = self.key_map.get(&key) {
            let value = self.items[idx].1;
            let item = self.items.remove(idx);
            self.items.insert(0, item);
            for i in 0..self.items.len() {
                self.key_map.insert(self.items[i].0, i);
            }
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
            self.key_map.remove(&self.items[0].0);
            self.items.remove(0);
        }
        self.items.insert(0, (key, value));
        for i in 0..self.items.len() {
            self.key_map.insert(self.items[i].0, i);
        }
        print!("Cache after put({}, {}): [", key, value);
        for (i, &(k, v)) in self.items.iter().enumerate() {
            print!("({}, {}){}", k, v, if i < self.items.len() - 1 { ", " } else { "" });
        }
        println!("]");
    }
}

fn main() {
    let mut cache = MRUCache::new(3);
    cache.put(1, 10);
    cache.put(2, 20);
    cache.put(3, 30);
    cache.put(4, 40);
    println!("Get(2) = {}", cache.get(2));
    cache.put(5, 50);
}
