package main

import "fmt"
import "math"

type Dual struct {
    value, deriv float64
}

func add(a, b Dual) Dual {
    return Dual{a.value + b.value, a.deriv + b.deriv}
}

func mul(a, b Dual) Dual {
    return Dual{a.value * b.value, a.deriv * b.value + a.value * b.deriv}
}

func sin(a Dual) Dual {
    return Dual{math.Sin(a.value), math.Cos(a.value) * a.deriv}
}

func main() {
    x := Dual{3.14, 1.0}
    x2 := mul(x, x)
    sx := sin(x)
    f := mul(x2, sx)
    fmt.Printf("f(x) = %.2f, f'(x) = %.2f\n", f.value, f.deriv)
}
