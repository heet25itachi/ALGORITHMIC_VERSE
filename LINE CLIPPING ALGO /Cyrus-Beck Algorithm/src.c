#include <stdio.h>
#include <stdbool.h>
#include <float.h>

typedef struct { double x, y; } Point;

const Point vertices[] = {{0, 0}, {10, 0}, {10, 10}, {0, 10}};
const int n_vertices = 4;

Point compute_normal(int i) {
    Point v1 = vertices[i], v2 = vertices[(i + 1) % n_vertices];
    Point normal = {-(v2.y - v1.y), v2.x - v1.x}; // Outward normal
    return normal;
}

double dot_product(Point a, Point b) {
    return a.x * b.x + a.y * b.y;
}

bool cyrus_beck_clip(Point *p0, Point *p1) {
    Point D = {p1->x - p0->x, p1->y - p0->y};
    if (D.x == 0 && D.y == 0) return false;

    double tE = 0.0, tL = 1.0;
    for (int i = 0; i < n_vertices; i++) {
        Point normal = compute_normal(i);
        Point PE = vertices[i];
        Point diff = {p0->x - PE.x, p0->y - PE.y};
        double num = -dot_product(normal, diff);
        double den = dot_product(normal, D);
        if (den == 0) continue; // Parallel to edge
        double t = num / den;
        if (den > 0) { // Exit point
            if (t < tL) tL = t;
        } else { // Entry point
            if (t > tE) tE = t;
        }
    }
    if (tE > tL || tE < 0 || tE > 1 || tL < 0 || tL > 1) return false;

    Point new_p0 = {p0->x + tE * D.x, p0->y + tE * D.y};
    Point new_p1 = {p0->x + tL * D.x, p0->y + tL * D.y};
    *p0 = new_p0;
    *p1 = new_p1;
    return true;
}

int main() {
    Point tests[][2] = {{{2, 2}, {8, 8}}, {{12, 12}, {15, 15}}, {{5, 12}, {15, 5}}, {{-5, 5}, {15, 5}}};
    for (int i = 0; i < 4; i++) {
        Point p0 = tests[i][0], p1 = tests[i][1];
        printf("Line from (%.1f, %.1f) to (%.1f, %.1f): ", p0.x, p0.y, p1.x, p1.y);
        if (cyrus_beck_clip(&p0, &p1)) {
            printf("Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", p0.x, p0.y, p1.x, p1.y);
        } else {
            printf("Rejected\n");
        }
    }
    return 0;
}
