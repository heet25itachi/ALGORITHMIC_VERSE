public class LiangBarsky {
    static class Point {
        double x, y;
        Point(double x, double y) { this.x = x; this.y = y; }
    }

    static boolean liangBarskyClip(double xmin, double ymin, double xmax, double ymax, Point p0, Point p1) {
        if (xmin >= xmax || ymin >= ymax) return false;

        double dx = p1.x - p0.x, dy = p1.y - p0.y;
        double[] p = {-dx, dx, -dy, dy};
        double[] q = {p0.x - xmin, xmax - p0.x, p0.y - ymin, ymax - p0.y};
        double u1 = 0.0, u2 = 1.0;

        for (int i = 0; i < 4; i++) {
            if (p[i] == 0 && q[i] < 0) return false;
            if (p[i] != 0) {
                double t = q[i] / p[i];
                if (p[i] < 0) {
                    if (t > u1) u1 = t;
                } else {
                    if (t < u2) u2 = t;
                }
            }
        }
        if (u1 > u2) return false;

        p0.x = p0.x + u1 * dx; p0.y = p0.y + u1 * dy;
        p1.x = p0.x + u2 * dx; p1.y = p0.y + u2 * dy;
        return true;
    }

    public static void main(String[] args) {
        double xmin = 0, ymin = 0, xmax = 10, ymax = 10;
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
            if (liangBarskyClip(xmin, ymin, xmax, ymax, p0, p1)) {
                System.out.printf("Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", p0.x, p0.y, p1.x, p1.y);
            } else {
                System.out.println("Rejected");
            }
        }
    }
}
