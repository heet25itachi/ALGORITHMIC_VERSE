const INSIDE: i32 = 0;
const LEFT: i32 = 1;
const RIGHT: i32 = 2;
const BOTTOM: i32 = 4;
const TOP: i32 = 8;

const XMIN: f64 = 0.0;
const YMIN: f64 = 0.0;
const XMAX: f64 = 10.0;
const YMAX: f64 = 10.0;

fn compute_outcode(x: f64, y: f64) -> i32 {
    let mut code = INSIDE;
    if x < XMIN {
        code |= LEFT;
    } else if x > XMAX {
        code |= RIGHT;
    }
    if y < YMIN {
        code |= BOTTOM;
    } else if y > YMAX {
        code |= TOP;
    }
    code
}

fn cohen_sutherland_clip(x0: &mut f64, y0: &mut f64, x1: &mut f64, y1: &mut f64) -> bool {
    let mut outcode0 = compute_outcode(*x0, *y0);
    let mut outcode1 = compute_outcode(*x1, *y1);
    let mut accept = false;

    loop {
        if outcode0 | outcode1 == 0 {
            accept = true;
            break;
        } else if outcode0 & outcode1 != 0 {
            break;
        } else {
            let mut x = 0.0;
            let mut y = 0.0;
            let outcode_out = if outcode1 > outcode0 { outcode1 } else { outcode0 };
            if outcode_out & TOP != 0 {
                x = *x0 + (*x1 - *x0) * (YMAX - *y0) / (*y1 - *y0);
                y = YMAX;
            } else if outcode_out & BOTTOM != 0 {
                x = *x0 + (*x1 - *x0) * (YMIN - *y0) / (*y1 - *y0);
                y = YMIN;
            } else if outcode_out & RIGHT != 0 {
                y = *y0 + (*y1 - *y0) * (XMAX - *x0) / (*x1 - *x0);
                x = XMAX;
            } else {
                y = *y0 + (*y1 - *y0) * (XMIN - *x0) / (*x1 - *x0);
                x = XMIN;
            }
            if outcode_out == outcode0 {
                *x0 = x;
                *y0 = y;
                outcode0 = compute_outcode(*x0, *y0);
            } else {
                *x1 = x;
                *y1 = y;
                outcode1 = compute_outcode(*x1, *y1);
            }
        }
    }
    accept
}

fn main() {
    let tests = [
        [2.0, 2.0, 8.0, 8.0],
        [12.0, 12.0, 15.0, 15.0],
        [5.0, 12.0, 15.0, 5.0],
        [-5.0, 5.0, 15.0, 5.0],
    ];
    for test in tests.iter() {
        let (mut x0, mut y0, mut x1, mut y1) = (test[0], test[1], test[2], test[3]);
        print!("Line from ({:.1}, {:.1}) to ({:.1}, {:.1}): ", x0, y0, x1, y1);
        if cohen_sutherland_clip(&mut x0, &mut y0, &mut x1, &mut y1) {
            println!("Accepted, clipped to ({:.1}, {:.1}) to ({:.1}, {:.1})", x0, y0, x1, y1);
        } else {
            println!("Rejected");
        }
    }
}
