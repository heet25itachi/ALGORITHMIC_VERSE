case class Point(x: Double, y: Double)

object CyrusBeck {
  val vertices = Array(Point(0, 0), Point(10, 0), Point(10, 10), Point(0, 10))
  val nVertices = 4

  def computeNormal(i: Int): Point = {
    val v1 = vertices(i)
    val v2 = vertices((i + 1) % nVertices)
    Point(-(v2.y - v1.y), v2.x - v1.x)
  }

  def dotProduct(a: Point, b: Point): Double = {
    a.x * b.x + a.y * b.y
  }

  def cyrusBeckClip(p0: Point, p1: Point): (Boolean, Point, Point) = {
    val D = Point(p1.x - p0.x, p1.y - p0.y)
    if (D.x == 0 && D.y == 0) return (false, p0, p1)

    var tE = 0.0
    var tL = 1.0
    for (i <- 0 until nVertices) {
      val normal = computeNormal(i)
      val PE = vertices(i)
      val diff = Point(p0.x - PE.x, p0.y - PE.y)
      val num = -dotProduct(normal, diff)
      val den = dotProduct(normal, D)
      if (den == 0) continue
      val t = num / den
      if (den > 0) {
        if (t < tL) tL = t
      } else {
        if (t > tE) tE = t
      }
    }
    if (tE > tL || tE < 0 || tE > 1 || tL < 0 || tL > 1) return (false, p0, p1)

    val p0New = Point(p0.x + tE * D.x, p0.y + tE * D.y)
    val p1New = Point(p0.x + tL * D.x, p0.y + tL * D.y)
    (true, p0New, p1New)
  }

  def main(args: Array[String]): Unit = {
    val tests = List(
      Array(Point(2, 2), Point(8, 8)),
      Array(Point(12, 12), Point(15, 15)),
      Array(Point(5, 12), Point(15, 5)),
      Array(Point(-5, 5), Point(15, 5))
    )
    for (test <- tests) {
      val p0 = test(0)
      val p1 = test(1)
      print(s"Line from (${p0.x}, ${p0.y}) to (${p1.x}, ${p1.y}): ")
      val (accept, p0New, p1New) = cyrusBeckClip(p0, p1)
      if (accept) {
        println(s"Accepted, clipped to (${p0New.x}, ${p0New.y}) to (${p1New.x}, ${p1New.y})")
      } else {
        println("Rejected")
      }
    }
  }
}
