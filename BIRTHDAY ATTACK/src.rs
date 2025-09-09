use rand::Rng;
use std::collections::HashMap;

const R: i64 = 65536;

fn birthday_attack() {
    let mut table: HashMap<i64, i64> = HashMap::new();
    let mut rng = rand::thread_rng();
    let mut trials = 0;
    loop {
        let x = rng.gen::<i64>();
        let h = x % R;
        trials += 1;
        if let Some(&prev_x) = table.get(&h) {
            if prev_x != x {
                println!("Collision found after {} trials: x_i = {}, x_j = {}, hash = {}", 
                         trials, prev_x, x, h);
                break;
            }
        }
        table.insert(h, x);
    }
}

fn main() {
    birthday_attack();
}
