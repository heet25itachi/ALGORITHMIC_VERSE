case class Dual(value: Double, deriv: Double) {
    def + (b: Dual) = Dual(value + b.value, deriv + b.deriv)
    def * (b: Dual) = Dual(value * b.value, deriv * b.value + value * b.deriv)
}

def sin(a: Dual) = Dual(math.sin(a.value), math.cos(a.value) * a.deriv)

val x = Dual(3.14, 1.0)
val x2 = x * x
val sx = sin(x)
val f = x2 * sx
println(f"f(x) = ${f.value}%.2f, f'(x) = ${f.deriv}%.2f")
