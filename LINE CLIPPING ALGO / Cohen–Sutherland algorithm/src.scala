object CohenSutherland {
  val INSIDE = 0
  val LEFT = 1
  val RIGHT = 2
  val BOTTOM = 4
  val TOP = 8

  val XMIN = 0.0
  val YMIN = 0.0
  val XMAX = 10.0
  val YMAX = 10.0

  def computeOutcode(x: Double, y: Double): Int = {
    var code = INSIDE
    if (x < XMIN) code |= LEFT
    else if (x > XMAX) code |= RIGHT
    if (y < YMIN) code |= BOTTOM
    else if (y > YMAX) code |= TOP
    code
  }

  def cohenSutherlandClip(x0: Double, y0: Double, x1: Double, y1: Double): (Boolean, Double, Double, Double, Double) = {
    var x0_ = x0
    var y0_ = y0
    var x1_ = x1
    var y1_ = y1
    var outcode0 = computeOutcode(x0_, y0_)
    var outcode1 = computeOutcode(x1_, y1_)
    var accept = false

    while (true) {
      if ((outcode0 | outcode1) == 0) {
        accept = true
        return (accept, x0_, y0_, x1_, y1_)
      } else if ((outcode0 & outcode1) != 0) {
        return (accept, x0_, y0_, x1_, y1_)
      } else {
        var x = 0.0
        var y = 0.0
        val outcodeOut = if (outcode1 > outcode0) outcode1 else outcode0
        if ((outcodeOut & TOP) != 0) {
          x = x0_ + (x1_ - x0_) * (YMAX - y0_) / (y1_ - y0_)
          y = YMAX
        } else if ((outcodeOut & BOTTOM) != 0) {
          x = x0_ + (x1_ - x0_) * (YMIN - y0_) / (y1_ - y0_)
          y = YMIN
        } else if ((outcodeOut & RIGHT) != 0) {
          y = y0_ + (y1_ - y0_) * (XMAX - x0_) / (x1_ - x0_)
          x = XMAX
        } else {
          y = y0_ + (y1_ - y0_) * (XMIN - x0_) / (x1_ - x0_)
          x = XMIN
        }
        if (outcodeOut == outcode0) {
          x0_ = x; y0_ = y
          outcode0 = computeOutcode(x0_, y0_)
        } else {
          x1_ = x; y1_ = y
          outcode1 = computeOutcode(x1_, y1_)
        }
      }
    }
    (accept, x0_, y0_, x1_, y1_)
  }

  def main(args: Array[String]): Unit = {
    val tests = List(
      Array(2.0, 2.0, 8.0, 8.0),
      Array(12.0, 12.0, 15.0, 15.0),
      Array(5.0, 12.0, 15.0, 5.0),
      Array(-5.0, 5.0, 15.0, 5.0)
    )
    for (test <- tests) {
      val (x0, y0, x1, y1) = (test(0), test(1), test(2), test(3))
      print(s"Line from ($x0, $y0) to ($x1, $y1): ")
      val (accept, x0_, y0_, x1_, y1_) = cohenSutherlandClip(x0, y0, x1, y1)
      if (accept) {
        println(s"Accepted, clipped to ($x0_, $y0_) to ($x1_, $y1_)")
      } else {
        println("Rejected")
      }
    }
  }
}
