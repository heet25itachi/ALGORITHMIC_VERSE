#include <stdio.h>
#include <stdbool.h>
#include <float.h>

typedef struct { double x, y; } Point;

bool liang_barsky_clip(double xmin, double ymin, double xmax, double ymax, Point *p0, Point *p1) {
    if (xmin >= xmax || ymin >= ymax) return false;

    double dx = p1->x - p0->x, dy = p1->y - p0->y;
    double p[4] = {-dx, dx, -dy, dy};
    double q[4] = {p0->x - xmin, xmax - p0->x, p0->y - ymin, ymax - p0->y};
    double u1 = 0.0, u2 = 1.0;

    for (int i = 0; i < 4; i++) {
        if (p[i] == 0 && q[i] < 0) return false; // Parallel and outside
        if (p[i] != 0) {
            double t = q[i] / p[i];
            if (p[i] < 0) {
                if (t > u1) u1 = t; // Entry point
            } else {
                if (t < u2) u2 = t; // Exit point
            }
        }
    }
    if (u1 > u2) return false; // No valid segment

    Point new_p0 = {p0->x + u1 * dx, p0->y + u1 * dy};
    Point new_p1 = {p0->x + u2 * dx, p0->y + u2 * dy};
    *p0 = new_p0;
    *p1 = new_p1;
    return true;
}

int main() {
    double xmin = 0, ymin = 0, xmax = 10, ymax = 10;
    Point tests[][2] = {{{2, 2}, {8, 8}}, {{12, 12}, {15, 15}}, {{5, 12}, {15, 5}}, {{-5, 5}, {15, 5}}};
    for (int i = 0; i < 4; i++) {
        Point p0 = tests[i][0], p1 = tests[i][1];
        printf("Line from (%.1f, %.1f) to (%.1f, %.1f): ", p0.x, p0.y, p1.x, p1.y);
        if (liang_barsky_clip(xmin, ymin, xmax, ymax, &p0, &p1)) {
            printf("Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", p0.x, p0.y, p1.x, p1.y);
        } else {
            printf("Rejected\n");
        }
    }
    return 0;
}
