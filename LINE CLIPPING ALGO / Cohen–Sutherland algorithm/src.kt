object CohenSutherland {
    private const val INSIDE = 0
    private const val LEFT = 1
    private const val RIGHT = 2
    private const val BOTTOM = 4
    private const val TOP = 8

    private const val XMIN = 0.0
    private const val YMIN = 0.0
    private const val XMAX = 10.0
    private const val YMAX = 10.0

    private fun computeOutcode(x: Double, y: Double): Int {
        var code = INSIDE
        if (x < XMIN) code = code or LEFT
        else if (x > XMAX) code = code or RIGHT
        if (y < YMIN) code = code or BOTTOM
        else if (y > YMAX) code = code or TOP
        return code
    }

    fun cohenSutherlandClip(x0: Double, y0: Double, x1: Double, y1: Double): Pair<Boolean, Pair<DoubleArray, DoubleArray>> {
        var x0 = x0
        var y0 = y0
        var x1 = x1
        var y1 = y1
        var outcode0 = computeOutcode(x0, y0)
        var outcode1 = computeOutcode(x1, y1)
        var accept = false

        while (true) {
            if (outcode0 or outcode1 == 0) {
                accept = true
                break
            } else if (outcode0 and outcode1 != 0) {
                break
            } else {
                var x = 0.0
                var y = 0.0
                val outcodeOut = if (outcode1 > outcode0) outcode1 else outcode0
                if (outcodeOut and TOP != 0) {
                    x = x0 + (x1 - x0) * (YMAX - y0) / (y1 - y0)
                    y = YMAX
                } else if (outcodeOut and BOTTOM != 0) {
                    x = x0 + (x1 - x0) * (YMIN - y0) / (y1 - y0)
                    y = YMIN
                } else if (outcodeOut and RIGHT != 0) {
                    y = y0 + (y1 - y0) * (XMAX - x0) / (x1 - x0)
                    x = XMAX
                } else {
                    y = y0 + (y1 - y0) * (XMIN - x0) / (x1 - x0)
                    x = XMIN
                }
                if (outcodeOut == outcode0) {
                    x0 = x
                    y0 = y
                    outcode0 = computeOutcode(x0, y0)
                } else {
                    x1 = x
                    y1 = y
                    outcode1 = computeOutcode(x1, y1)
                }
            }
        }
        return Pair(accept, Pair(doubleArrayOf(x0, y0), doubleArrayOf(x1, y1)))
    }
}

fun main() {
    val tests = listOf(
        doubleArrayOf(2.0, 2.0, 8.0, 8.0),
        doubleArrayOf(12.0, 12.0, 15.0, 15.0),
        doubleArrayOf(5.0, 12.0, 15.0, 5.0),
        doubleArrayOf(-5.0, 5.0, 15.0, 5.0)
    )
    for (test in tests) {
        val (x0, y0, x1, y1) = test
        print("Line from ($x0, $y0) to ($x1, $y1): ")
        val (accept, points) = CohenSutherland.cohenSutherlandClip(x0, y0, x1, y1)
        if (accept) {
            println("Accepted, clipped to (${points[0][0]}, ${points[0][1]}) to (${points[1][0]}, ${points[1][1]})")
        } else {
            println("Rejected")
        }
    }
}
