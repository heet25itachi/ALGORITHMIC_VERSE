public class CohenSutherland {
    private static final int INSIDE = 0, LEFT = 1, RIGHT = 2, BOTTOM = 4, TOP = 8;
    private static final double XMIN = 0.0, YMIN = 0.0, XMAX = 10.0, YMAX = 10.0;

    static class Point {
        double x, y;
        Point(double x, double y) { this.x = x; this.y = y; }
    }

    private static int computeOutcode(double x, double y) {
        int code = INSIDE;
        if (x < XMIN) code |= LEFT;
        else if (x > XMAX) code |= RIGHT;
        if (y < YMIN) code |= BOTTOM;
        else if (y > YMAX) code |= TOP;
        return code;
    }

    public static boolean cohenSutherlandClip(Point p0, Point p1) {
        int outcode0 = computeOutcode(p0.x, p0.y);
        int outcode1 = computeOutcode(p1.x, p1.y);
        boolean accept = false;

        while (true) {
            if ((outcode0 | outcode1) == 0) {
                accept = true;
                break;
            } else if ((outcode0 & outcode1) != 0) {
                break;
            } else {
                double x, y;
                int outcodeOut = outcode1 > outcode0 ? outcode1 : outcode0;
                if ((outcodeOut & TOP) != 0) {
                    x = p0.x + (p1.x - p0.x) * (YMAX - p0.y) / (p1.y - p0.y);
                    y = YMAX;
                } else if ((outcodeOut & BOTTOM) != 0) {
                    x = p0.x + (p1.x - p0.x) * (YMIN - p0.y) / (p1.y - p0.y);
                    y = YMIN;
                } else if ((outcodeOut & RIGHT) != 0) {
                    y = p0.y + (p1.y - p0.y) * (XMAX - p0.x) / (p1.x - p0.x);
                    x = XMAX;
                } else {
                    y = p0.y + (p1.y - p0.y) * (XMIN - p0.x) / (p1.x - p0.x);
                    x = XMIN;
                }
                if (outcodeOut == outcode0) {
                    p0.x = x; p0.y = y;
                    outcode0 = computeOutcode(p0.x, p0.y);
                } else {
                    p1.x = x; p1.y = y;
                    outcode1 = computeOutcode(p1.x, p1.y);
                }
            }
        }
        return accept;
    }

    public static void main(String[] args) {
        double[][] tests = {{2, 2, 8, 8}, {12, 12, 15, 15}, {5, 12, 15, 5}, {-5, 5, 15, 5}};
        for (double[] test : tests) {
            Point p0 = new Point(test[0], test[1]);
            Point p1 = new Point(test[2], test[3]);
            System.out.printf("Line from (%.1f, %.1f) to (%.1f, %.1f): ", p0.x, p0.y, p1.x, p1.y);
            if (cohenSutherlandClip(p0, p1)) {
                System.out.printf("Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", p0.x, p0.y, p1.x, p1.y);
            } else {
                System.out.println("Rejected");
            }
        }
    }
}
