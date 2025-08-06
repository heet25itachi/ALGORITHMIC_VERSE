package main

import (
    "fmt"
    "math"
)

type Point struct {
    x, y float64
}

var vertices = []Point{{0, 0}, {10, 0}, {10, 10}, {0, 10}}
const nVertices = 4

func computeNormal(i int) Point {
    v1, v2 := vertices[i], vertices[(i+1)%nVertices]
    return Point{-(v2.y - v1.y), v2.x - v1.x}
}

func dotProduct(a, b Point) float64 {
    return a.x*b.x + a.y*b.y
}

func cyrusBeckClip(p0, p1 *Point) bool {
    D := Point{p1.x - p0.x, p1.y - p0.y}
    if D.x == 0 && D.y == 0 {
        return false
    }

    tE, tL := 0.0, 1.0
    for i := 0; i < nVertices; i++ {
        normal := computeNormal(i)
        PE := vertices[i]
        diff := Point{p0.x - PE.x, p0.y - PE.y}
        num := -dotProduct(normal, diff)
        den := dotProduct(normal, D)
        if den == 0 {
            continue
        }
        t := num / den
        if den > 0 {
            if t < tL {
                tL = t
            }
        } else {
            if t > tE {
                tE = t
            }
        }
    }
    if tE > tL || tE < 0 || tE > 1 || tL < 0 || tL > 1 {
        return false
    }

    p0.x, p0.y = p0.x+tE*D.x, p0.y+tE*D.y
    p1.x, p1.y = p0.x+tL*D.x, p0.y+tL*D.y
    return true
}

func main() {
    tests := [][2]Point{{{2, 2}, {8, 8}}, {{12, 12}, {15, 15}}, {{5, 12}, {15, 5}}, {{-5, 5}, {15, 5}}}
    for _, test := range tests {
        p0, p1 := test[0], test[1]
        fmt.Printf("Line from (%.1f, %.1f) to (%.1f, %.1f): ", p0.x, p0.y, p1.x, p1.y)
        if cyrusBeckClip(&p0, &p1) {
            fmt.Printf("Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", p0.x, p0.y, p1.x, p1.y)
        } else {
            fmt.Println("Rejected")
        }
    }
}
