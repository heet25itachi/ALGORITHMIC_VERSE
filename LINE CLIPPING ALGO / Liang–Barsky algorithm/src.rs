#[derive(Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

fn liang_barsky_clip(xmin: f64, ymin: f64, xmax: f64, ymax: f64, p0: &mut Point, p1: &mut Point) -> bool {
    if xmin >= xmax || ymin >= ymax {
        return false;
    }

    let dx = p1.x - p0.x;
    let dy = p1.y - p0.y;
    let p = [-dx, dx, -dy, dy];
    let q = [p0.x - xmin, xmax - p0.x, p0.y - ymin, ymax - p0.y];
    let mut u1 = 0.0;
    let mut u2 = 1.0;

    for i in 0..4 {
        if p[i] == 0.0 && q[i] < 0.0 {
            return false;
        }
        if p[i] != 0.0 {
            let t = q[i] / p[i];
            if p[i] < 0.0 {
                if t > u1 {
                    u1 = t;
                }
            } else {
                if t < u2 {
                    u2 = t;
                }
            }
        }
    }
    if u1 > u2 {
        return false;
    }

    let p0_new = Point { x: p0.x + u1 * dx, y: p0.y + u1 * dy };
    let p1_new = Point { x: p0.x + u2 * dx, y: p0.y + u2 * dy };
    *p0 = p0_new;
    *p1 = p1_new;
    true
}

fn main() {
    let xmin = 0.0;
    let ymin = 0.0;
    let xmax = 10.0;
    let ymax = 10.0;
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
        if liang_barsky_clip(xmin, ymin, xmax, ymax, &mut p0, &mut p1) {
            println!("Accepted, clipped to ({:.1}, {:.1}) to ({:.1}, {:.1})", p0.x, p0.y, p1.x, p1.y);
        } else {
            println!("Rejected");
        }
    }
}
