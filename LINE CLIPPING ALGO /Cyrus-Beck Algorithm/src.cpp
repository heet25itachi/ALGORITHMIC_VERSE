#include <iostream>
#include <vector>
using namespace std;

struct Point { double x, y; };

const vector<Point> vertices = {{0, 0}, {10, 0}, {10, 10}, {0, 10}};
const int n_vertices = 4;

Point compute_normal(int i) {
    Point v1 = vertices[i], v2 = vertices[(i + 1) % n_vertices];
    return {-(v2.y - v1.y), v2.x - v1.x}; // Outward normal
}

double dot_product(Point a, Point b) {
    return a.x * b.x + a.y * b.y;
}

bool cyrus_beck_clip(Point& p0, Point& p1) {
    Point D = {p1.x - p0.x, p1.y - p0.y};
    if (D.x == 0 && D.y == 0) return false;

    double tE = 0.0, tL = 1.0;
    for (int i = 0; i < n_vertices; i++) {
        Point normal = compute_normal(i);
        Point PE = vertices[i];
        Point diff = {p0.x - PE.x, p0.y - PE.y};
        double num = -dot_product(normal, diff);
        double den = dot_product(normal, D);
        if (den == 0) continue;
        double t = num / den;
        if (den > 0) {
            if (t < tL) tL = t;
        } else {
            if (t > tE) tE = t;
        }
    }
    if (tE > tL || tE < 0 || tE > 1 || tL < 0 || tL > 1) return false;

    p0 = {p0.x + tE * D.x, p0.y + tE * D.y};
    p1 = {p0.x + tL * D.x, p0.y + tL * D.y};
    return true;
}

int main() {
    vector<pair<Point, Point>> tests = {{{2, 2}, {8, 8}}, {{12, 12}, {15, 15}}, {{5, 12}, {15, 5}}, {{-5, 5}, {15, 5}}};
    for (const auto& test : tests) {
        Point p0 = test.first, p1 = test.second;
        cout << "Line from (" << p0.x << ", " << p0.y << ") to (" << p1.x << ", " << p1.y << "): ";
        if (cyrus_beck_clip(p0, p1)) {
            cout << "Accepted, clipped to (" << p0.x << ", " << p0.y << ") to (" << p1.x << ", " << p1.y << ")\n";
        } else {
            cout << "Rejected\n";
        }
    }
    return 0;
}
