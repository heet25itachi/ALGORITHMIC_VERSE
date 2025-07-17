object Difference {
  def y(k: Double, steps: Int): Double = {
    var result = 1.0 // y(0) = 1
    for (i <- 0 until steps) {
      result *= i // y(k+1) = k * y(k)
    }
    result
  }

  def main(args: Array[String]): Unit = {
    val k = 5
    println(s"Numerical solution for y($k) = ${y(k, k)}")
  }
}
