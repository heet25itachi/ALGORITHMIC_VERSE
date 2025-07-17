fun y(k: Double, steps: Int): Double {
    var result = 1.0 // y(0) = 1
    for (i in 0 until steps) {
        result *= i.toDouble() // y(k+1) = k * y(k)
    }
    return result
}

fun main() {
    val k = 5
    println("Numerical solution for y($k) = ${y(k.toDouble(), k)}")
}
