package main

import "fmt"

const (
    INSIDE = 0
    LEFT   = 1
    RIGHT  = 2
    BOTTOM = 4
    TOP    = 8
)

const (
    xmin = 0.0
    ymin = 0.0
    xmax = 10.0
    ymax = 10.0
)

func computeOutcode(x, y float64) int {
    code := INSIDE
    if x < xmin {
        code |= LEFT
    } else if x > xmax {
        code |= RIGHT
    }
    if y < ymin {
        code |= BOTTOM
    } else if y > ymax {
        code |= TOP
    }
    return code
}

func cohenSutherlandClip(x0, y0, x1, y1 *float64) bool {
    outcode0 := computeOutcode(*x0, *y0)
    outcode1 := computeOutcode(*x1, *y1)
    accept := false

    for {
        if outcode0|outcode1 == 0 {
            accept = true
            break
        } else if outcode0&outcode1 != 0 {
            break
        } else {
            var x, y float64
            outcodeOut := outcode1
            if outcode0 > outcode1 {
                outcodeOut = outcode0
            }
            if outcodeOut&TOP != 0 {
                x = *x0 + (*x1-*x0)*(ymax-*y0)/(*y1-*y0)
                y = ymax
            } else if outcodeOut&BOTTOM != 0 {
                x = *x0 + (*x1-*x0)*(ymin-*y0)/(*y1-*y0)
                y = ymin
            } else if outcodeOut&RIGHT != 0 {
                y = *y0 + (*y1-*y0)*(xmax-*x0)/(*x1-*x0)
                x = xmax
            } else {
                y = *y0 + (*y1-*y0)*(xmin-*x0)/(*x1-*x0)
                x = xmin
            }
            if outcodeOut == outcode0 {
                *x0, *y0 = x, y
                outcode0 = computeOutcode(*x0, *y0)
            } else {
                *x1, *y1 = x, y
                outcode1 = computeOutcode(*x1, *y1)
            }
        }
    }
    return accept
}

func main() {
    tests := [][4]float64{{2, 2, 8, 8}, {12, 12, 15, 15}, {5, 12, 15, 5}, {-5, 5, 15, 5}}
    for _, test := range tests {
        x0, y0, x1, y1 := test[0], test[1], test[2], test[3]
        fmt.Printf("Line from (%.1f, %.1f) to (%.1f, %.1f): ", x0, y0, x1, y1)
        if cohenSutherlandClip(&x0, &y0, &x1, &y1) {
            fmt.Printf("Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", x0, y0, x1, y1)
        } else {
            fmt.Println("Rejected")
        }
    }
}
