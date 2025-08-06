data class Point(var x: Double, var y: Double)

object LiangBarsky {
    fun liangBarskyClip(xmin: Double, ymin: Double, xmax: Double, ymax: Double, p0: Point, p1: Point): Boolean {
        if (xmin >= xmax || ymin >= ymax) return false

        val dx = p1.x - p0.x
        val dy = p1.y - p0.y
        val p = doubleArrayOf(-dx, dx, -dy, dy)
        val q = doubleArrayOf(p0.x - xmin, xmax - p0.x, p0.y - ymin, ymax - p0.y)
        var u1 = 0.0
        var u2 = 1.0

        for (i in 0 until 4) {
            if (p[i] == 0.0 && q[i] < 0.0) return false
            if (p[i] != 0.0) {
                val t = q[i] / p[i]
                if (p[i] < 0) {
                    if (t > u1) u1 = t
                } else {
                    if (t < u2) u2 = t
                }
            }
        }
        if (u1 > u2) return false

        p0.x = p0.x + u1 * dx; p0.y = p0.y + u1 * dy
        p1.x = p0.x + u2 * dx; p1.y = p0.y + u2 * dy
        return true
    }
}

fun main() {
    val xmin = 0.0
    val ymin = 0.0
    val xmax = 10.0
    val ymax = 10.0
    val tests = listOf(
        arrayOf(Point(2.0, 2.0), Point(8.0, 8.0)),
        arrayOf(Point(12.0, 12.0), Point(15.0, 15.0)),
        arrayOf(Point(5.0, 12.0), Point(15.0, 5.0)),
        arrayOf(Point(-5.0, 5.0), Point(15.0, 5.0))
    )
    for (test in tests) {
        val p0 = Point(test[0].x, test[0].y)
        val p1 = Point(test[1].x, test[1].y)
        print("Line from (${p0.x}, ${p0.y}) to (${p1.x}, ${p1.y}): ")
        if (LiangBarsky.liangBarskyClip(xmin, ymin, xmax, ymax, p0, p1)) {
            println("Accepted, clipped to (${p0.x}, ${p0.y}) to (${p1.x}, ${p1.y})")
        } else {
            println("Rejected")
        }
    }
}
