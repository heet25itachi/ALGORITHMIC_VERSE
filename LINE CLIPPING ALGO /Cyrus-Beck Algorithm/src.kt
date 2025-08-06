data class Point(var x: Double, var y: Double)

object CyrusBeck {
    val vertices = arrayOf(Point(0.0, 0.0), Point(10.0, 0.0), Point(10.0, 10.0), Point(0.0, 10.0))
    const val nVertices = 4

    fun computeNormal(i: Int): Point {
        val v1 = vertices[i]
        val v2 = vertices[(i + 1) % nVertices]
        return Point(-(v2.y - v1.y), v2.x - v1.x)
    }

    fun dotProduct(a: Point, b: Point): Double {
        return a.x * b.x + a.y * b.y
    }

    fun cyrusBeckClip(p0: Point, p1: Point): Boolean {
        val D = Point(p1.x - p0.x, p1.y - p0.y)
        if (D.x == 0.0 && D.y == 0.0) return false

        var tE = 0.0
        var tL = 1.0
        for (i in 0 until nVertices) {
            val normal = computeNormal(i)
            val PE = vertices[i]
            val diff = Point(p0.x - PE.x, p0.y - PE.y)
            val num = -dotProduct(normal, diff)
            val den = dotProduct(normal, D)
            if (den == 0.0) continue
            val t = num / den
            if (den > 0) {
                if (t < tL) tL = t
            } else {
                if (t > tE) tE = t
            }
        }
        if (tE > tL || tE < 0.0 || tE > 1.0 || tL < 0.0 || tL > 1.0) return false

        p0.x = p0.x + tE * D.x; p0.y = p0.y + tE * D.y
        p1.x = p0.x + tL * D.x; p1.y = p0.y + tL * D.y
        return true
    }
}

fun main() {
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
        if (CyrusBeck.cyrusBeckClip(p0, p1)) {
            println("Accepted, clipped to (${p0.x}, ${p0.y}) to (${p1.x}, ${p1.y})")
        } else {
            println("Rejected")
        }
    }
}
