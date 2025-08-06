public class CyrusBeck {
    static class Point {
        double x, y;
        Point(double x, double y) { this.x = x; this.y = y; }
    }

    static final Point[] vertices = {new Point(0, 0), new Point(10, 0), new Point(10, 10), new Point(0, 10)};
    static final int n_vertices = 4;

    static Point computeNormal(int i) {
        Point v1 = vertices[i], v2 = vertices[(i + 1) % n_vertices];
        return new Point(-(v2.y - v1.y), v2.x - v1.x);
    }

    static double dotProduct(Point a, Point b) {
        return a.x * b.x + a.y * b.y;
    }

    static boolean cyrusBeckClip(Point p0, Point p1) {
        Point D = new Point(p1.x - p0.x, p1.y - p0.y);
        if (D.x == 0 && D.y == 0) return false;

        double tE = 0.0, tL = 1.0;
        for (int i = 0; i < n_vertices; i++) {
            Point normal = computeNormal(i);
            Point PE = vertices[i];
            Point diff = new Point(p0.x - PE.x, p0.y - PE.y);
            double num = -dotProduct(normal, diff);
            double den = dotProduct(normal, D);
            if (den == 0) continue;
            double t = num / den;
            if (den > 0) {
                if (t < tL) tL = t;
            } else {
                if (t > tE) tE = t;
            }
        }
        if (tE > tL || tE < 0 || tE > 1 || tL < 0 || tL > 1) return false;

        p0.x = p0.x + tE * D.x; p0.y = p0.y + tE * D.y;
        p1.x = p0.x + tL * D.x; p1.y = p0.y + tL * D.y;
        return true;
    }

    public static void main(String[] args) {
        Point[][] tests = {
            {new Point(2, 2), new Point(8, 8)},
            {new Point(12, 12), new Point(15, 15)},
            {new Point(5, 12), new Point(15, 5)},
            {new Point(-5, 5), new Point(15, 5)}
        };
        for (Point[] test : tests) {
            Point p0 = new Point(test[0].x, test[0].y);
            Point p1 = new Point(test[1].x, test[1].y);
            System.out.printf("Line from (%.1f, %.1f) to (%.1f, %.1f): ", p0.x, p0.y, p1.x, p1.y);
            if (cyrusBeckClip(p0, p1)) {
                System.out.printf("Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", p0.x, p0.y, p1.x, p1.y);
            } else {
                System.out.println("Rejected");
            }
        }
    }
}
