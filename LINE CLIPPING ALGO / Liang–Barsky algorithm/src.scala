case class Point(x: Double, y: Double)

object LiangBarsky {
  def liangBarskyClip(xmin: Double, ymin: Double, xmax: Double, ymax: Double, p0: Point, p1: Point): (Boolean, Point, Point) {
    if (xmin >= xmax || ymin >= ymax) return (false, p0, p1)

    val dx = p1.x - p0.x
    val dy = p1.y - p0.y
    val p = Array(-dx, dx, -dy, dy)
    val q = Array(p0.x - xmin, xmax - p0.x, p0.y - ymin, ymax - p0.y)
    var u1 = 0.0
    var u2 = 1.0

    for (i <- 0 until 4) {
      if (p(i) == 0 && q(i) < 0) return (false, p0, p1)
      if (p(i) != 0) {
        val t = q(i) / p(i)
        if (p(i) < 0) {
          if (t > u1) u1 = t
        } else {
          if (t < u2) u2 = t
        }
      }
    }
    if (u1 > u2) return (false, p0, p1)

    val p0New = Point(p0.x + u1 * dx, p0.y + u1 * dy)
    val p1New = Point(p0.x + u2 * dx, p0.y + u2 * dy)
    (true, p0New, p1New)
  }

  def main(args: Array[String]): Unit = {
    val xmin = 0.0
    val ymin = 0.0
    val xmax = 10.0
    val ymax = 10.0
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
      val (accept, p0New, p1New) = liangBarskyClip(xmin, ymin, xmax, ymax, p0, p1)
      if (accept) {
        println(s"Accepted, clipped to (${p0New.x}, ${p0New.y}) to (${p1New.x}, ${p1New.y})")
      } else {
        println("Rejected")
      }
    }
  }
}
