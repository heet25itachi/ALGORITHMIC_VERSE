#include <iostream>
#include <vector>
using namespace std;

struct Point { double x, y; };

bool liang_barsky_clip(double xmin, double ymin, double xmax, double ymax, Point& p0, Point& p1) {
    if (xmin >= xmax || ymin >= ymax) return false;

    double dx = p1.x - p0.x, dy = p1.y - p0.y;
    double p[4] = {-dx, dx, -dy, dy};
    double q[4] = {p0.x - xmin, xmax - p0.x, p0.y - ymin, ymax - p0.y};
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

    p0 = {p0.x + u1 * dx, p0.y + u1 * dy};
    p1 = {p0.x + u2 * dx, p0.y + u2 * dy};
    return true;
}

int main() {
    double xmin = 0, ymin = 0, xmax = 10, ymax = 10;
    vector<pair<Point, Point>> tests = {{{2, 2}, {8, 8}}, {{12, 12}, {15, 15}}, {{5, 12}, {15, 5}}, {{-5, 5}, {15, 5}}};
    for (const auto& test : tests) {
        Point p0 = test.first, p1 = test.second;
        cout << "Line from (" << p0.x << ", " << p0.y << ") to (" << p1.x << ", " << p1.y << "): ";
        if (liang_barsky_clip(xmin, ymin, xmax, ymax, p0, p1)) {
            cout << "Accepted, clipped to (" << p0.x << ", " << p0.y << ") to (" << p1.x << ", " << p1.y << ")\n";
        } else {
            cout << "Rejected\n";
        }
    }
    return 0;
}
