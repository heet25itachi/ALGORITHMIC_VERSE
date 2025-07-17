fn y(k: f64, steps: i32) -> f64 {
    let mut result = 1.0; // y(0) = 1
    for i in 0..steps {
        result *= i as f64; // y(k+1) = k * y(k)
    }
    result
}

fn main() {
    let k = 5;
    println!("Numerical solution for y({}) = {}", k, y(k as f64, k));
}
