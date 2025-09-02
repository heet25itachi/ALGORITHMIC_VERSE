data class Dual(val value: Double, val deriv: Double) {
    operator fun plus(b: Dual) = Dual(value + b.value, deriv + b.deriv)
    operator fun times(b: Dual) = Dual(value * b.value, deriv * b.value + value * b.deriv)
}

fun sin(a: Dual) = Dual(Math.sin(a.value), Math.cos(a.value) * a.deriv)

fun main() {
    val x = Dual(3.14, 1.0)
    val x2 = x * x
    val sx = sin(x)
    val f = x2 * sx
    println("f(x) = %.2f, f'(x) = %.2f".format(f.value, f.deriv))
}
