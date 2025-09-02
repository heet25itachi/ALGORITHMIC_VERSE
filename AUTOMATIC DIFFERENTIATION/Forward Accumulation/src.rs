use std::ops::{Add, Mul};

#[derive(Copy, Clone)]
struct Dual {
    value: f64,
    deriv: f64,
}

impl Add for Dual {
    type Output = Self;
    fn add(self, b: Self) -> Self {
        Dual { value: self.value + b.value, deriv: self.deriv + b.deriv }
    }
}

impl Mul for Dual {
    type Output = Self;
    fn mul(self, b: Self) -> Self {
        Dual { value: self.value * b.value, deriv: self.deriv * b.value + self.value * b.deriv }
    }
}

fn sin(a: Dual) -> Dual {
    Dual { value: a.value.sin(), deriv: a.value.cos() * a.deriv }
}

fn main() {
    let x = Dual { value: 3.14, deriv: 1.0 };
    let x2 = x * x;
    let sx = sin(x);
    let f = x2 * sx;
    println!("f(x) = {:.2}, f'(x) = {:.2}", f.value, f.deriv);
}
