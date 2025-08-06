#[derive(Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

const VERTICES: [Point; 4] = [Point { x: 0.0, y: 0.0 }, Point { x: 10.0, y: 0.0 }, Point { x: 10.0, y: 10.0 }, Point { x: 0.0, y: 10.0 }];
const N_VERTICES: usize = 4;

fn compute_normal(i: usize) -> Point {
    let v1 = VERTICES[i];
    let v2 = VERTICES[(i + 1) % N_VERTICES];
    Point { x: -(v2.y - v1.y), y: v2.x - v1.x }
}

fn dot_product(a: Point, b: Point) -> f64 {
    a.x * b.x + a.y * b.y
}

fn cyrus_beck_clip(p0: &mut Point, p1: &mut Point) -> bool {
    let D = Point { x: p1.x - p0.x, y: p1.y - p0.y };
    if D.x == 0.0 && D.y == 0.0 {
        return false;
    }

    let mut tE = 0.0;
    let mut tL = 1.0;
    for i in 0..N_VERTICES {
        let normal = compute_normal(i);
        let PE = VERTICES[i];
        let diff = Point { x: p0.x - PE.x, y: p0.y - PE.y };
        let num = -dot_product(normal, diff);
        let den = dot_product(normal, D);
        if den == 0.0 {
            continue;
        }
        let t = num / den;
        if den > 0.0 {
            if t < tL {
                tL = t;
            }
        } else {
            if t > tE {
                tE = t;
            }
        }
    }
    if tE > tL || tE < 0.0 || tE > 1.0 || tL < 0.0 || tL > 1.0 {
        return false;
    }

    let p0_new = Point { x: p0.x + tE * D.x, y: p0.y + tE * D.y };
    let p1_new = Point { x: p0.x + tL * D.x, y: p0.y + tL * D.y };
    *p0 = p0_new;
    *p1 = p1_new;
    true
}

fn main() {
    let tests = [
        [Point { x: 2.0, y: 2.0 }, Point { x: 8.0, y: 8.0 }],
        [Point { x: 12.0, y: 12.0 }, Point { x: 15.0, y: 15.0 }],
        [Point { x: 5.0, y: 12.0 }, Point { x: 15.0, y: 5.0 }],
        [Point { x: -5.0, y: 5.0 }, Point { x: 15.0, y: 5.0 }],
    ];
    for test in tests.iter() {
        let mut p0 = test[0];
        let mut p1 = test[1];
        print!("Line from ({:.1}, {:.1}) to ({:.1}, {:.1}): ", p0.x, p0.y, p1.x, p1.y);
        if cyrus_beck_clip(&mut p0, &mut p1) {
            println!("Accepted, clipped to ({:.1}, {:.1}) to ({:.1}, {:.1})", p0.x, p0.y, p1.x, p1.y);
        } else {
            println!("Rejected");
        }
    }
}
