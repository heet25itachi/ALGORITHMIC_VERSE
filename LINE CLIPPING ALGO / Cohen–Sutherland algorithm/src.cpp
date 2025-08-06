#include <iostream>
using namespace std;

const int INSIDE = 0, LEFT = 1, RIGHT = 2, BOTTOM = 4, TOP = 8;
const double xmin = 0.0, ymin = 0.0, xmax = 10.0, ymax = 10.0;

using OutCode = int;

OutCode compute_outcode(double x, double y) {
    OutCode code = INSIDE;
    if (x < xmin) code |= LEFT;
    else if (x > xmax) code |= RIGHT;
    if (y < ymin) code |= BOTTOM;
    else if (y > ymax) code |= TOP;
    return code;
}

bool cohen_sutherland_clip(double& x0, double& y0, double& x1, double& y1) {
    OutCode outcode0 = compute_outcode(x0, y0);
    OutCode outcode1 = compute_outcode(x1, y1);
    bool accept = false;

    while (true) {
        if (!(outcode0 | outcode1)) {
            accept = true;
            break;
        } else if (outcode0 & outcode1) {
            break;
        } else {
            double x, y;
            OutCode outcode_out = outcode1 > outcode0 ? outcode1 : outcode0;
            if (outcode_out & TOP) {
                x = x0 + (x1 - x0) * (ymax - y0) / (y1 - y0);
                y = ymax;
            } else if (outcode_out & BOTTOM) {
                x = x0 + (x1 - x0) * (ymin - y0) / (y1 - y0);
                y = ymin;
            } else if (outcode_out & RIGHT) {
                y = y0 + (y1 - y0) * (xmax - x0) / (x1 - x0);
                x = xmax;
            } else if (outcode_out & LEFT) {
                y = y0 + (y1 - y0) * (xmin - x0) / (x1 - x0);
                x = xmin;
            }
            if (outcode_out == outcode0) {
                x0 = x; y0 = y;
                outcode0 = compute_outcode(x0, y0);
            } else {
                x1 = x; y1 = y;
                outcode1 = compute_outcode(x1, y1);
            }
        }
    }
    return accept;
}

int main() {
    double tests[][4] = {{2, 2, 8, 8}, {12, 12, 15, 15}, {5, 12, 15, 5}, {-5, 5, 15, 5}};
    for (const auto& test : tests) {
        double x0 = test[0], y0 = test[1], x1 = test[2], y1 = test[3];
        cout << "Line from (" << x0 << ", " << y0 << ") to (" << x1 << ", " << y1 << "): ";
        if (cohen_sutherland_clip(x0, y0, x1, y1)) {
            cout << "Accepted, clipped to (" << x0 << ", " << y0 << ") to (" << x1 << ", " << y1 << ")\n";
        } else {
            cout << "Rejected\n";
        }
    }
    return 0;
}
