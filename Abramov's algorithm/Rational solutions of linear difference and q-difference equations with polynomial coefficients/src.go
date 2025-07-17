package main

import "fmt"

func y(k float64, steps int) float64 {
    result := 1.0 // y(0) = 1
    for i := 0; i < steps; i++ {
        result *= float64(i) // y(k+1) = k * y(k)
    }
    return result
}

func main() {
    k := 5
    fmt.Printf("Numerical solution for y(%d) = %f\n", k, y(float64(k), k))
}
