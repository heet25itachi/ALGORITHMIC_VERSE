package main

import (
    "fmt"
)

type Point struct {
    x, y float64
}

func liangBarskyClip(xmin, ymin, xmax, ymax float64, p0, p1 *Point) bool {
    if xmin >= xmax || ymin >= ymax {
        return false
    }

    dx := p1.x - p0.x
    dy := p1.y - p0.y
    p := []float64{-dx, dx, -dy, dy}
    q := []float64{p0.x - xmin, xmax - p0.x, p0.y - ymin, ymax - p0.y}
    u1, u2 := 0.0, 1.0

    for i := 0; i < 4; i++ {
        if p[i] == 0 && q[i] < 0 {
            return false
        }
        if p[i] != 0 {
            t := q[i] / p[i]
            if p[i] < 0 {
                if t > u1 {
                    u1 = t
                }
            } else {
                if t < u2 {
                    u2 = t
                }
            }
        }
    }
    if u1 > u2 {
        return false
    }

    p0.x, p0.y = p0.x+u1*dx, p0.y+u1*dy
    p1.x, p1.y = p0.x+u2*dx, p0.y+u2*dy
    return true
}

func main() {
    xmin, ymin, xmax, ymax := 0.0, 0.0, 10.0, 10.0
    tests := [][2]Point{{{2, 2}, {8, 8}}, {{12, 12}, {15, 15}}, {{5, 12}, {15, 5}}, {{-5, 5}, {15, 5}}}
    for _, test := range tests {
        p0, p1 := test[0], test[1]
        fmt.Printf("Line from (%.1f, %.1f) to (%.1f, %.1f): ", p0.x, p0.y, p1.x, p1.y)
        if liangBarskyClip(xmin, ymin, xmax, ymax, &p0, &p1) {
            fmt.Printf("Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", p0.x, p0.y, p1.x, p1.y)
        } else {
            fmt.Println("Rejected")
        }
    }
}
