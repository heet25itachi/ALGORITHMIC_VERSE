class Point:
    def __init__(self, x, y):
        self.x, self.y = x, y

def liang_barsky_clip(xmin, ymin, xmax, ymax, p0, p1):
    if xmin >= xmax or ymin >= ymax:
        return False, p0, p1

    dx = p1.x - p0.x
    dy = p1.y - p0.y
    p = [-dx, dx, -dy, dy]
    q = [p0.x - xmin, xmax - p0.x, p0.y - ymin, ymax - p0.y]
    u1, u2 = 0.0, 1.0

    for i in range(4):
        if p[i] == 0 and q[i] < 0:
            return False, p0, p1
        if p[i] != 0:
            t = q[i] / p[i]
            if p[i] < 0:
                if t > u1:
                    u1 = t
            else:
                if t < u2:
                    u2 = t
    if u1 > u2:
        return False, p0, p1

    p0_new = Point(p0.x + u1 * dx, p0.y + u1 * dy)
    p1_new = Point(p0.x + u2 * dx, p0.y + u2 * dy)
    return True, p0_new, p1_new

if __name__ == "__main__":
    xmin, ymin, xmax, ymax = 0, 0, 10, 10
    tests = [(2, 2, 8, 8), (12, 12, 15, 15), (5, 12, 15, 5), (-5, 5, 15, 5)]
    for x0, y0, x1, y1 in tests:
        p0, p1 = Point(x0, y0), Point(x1, y1)
        print(f"Line from ({x0:.1f}, {y0:.1f}) to ({x1:.1f}, {y1:.1f}): ", end="")
        accept, p0_new, p1_new = liang_barsky_clip(xmin, ymin, xmax, ymax, p0, p1)
        if accept:
            print(f"Accepted, clipped to ({p0_new.x:.1f}, {p0_new.y:.1f}) to ({p1_new.x:.1f}, {p1_new.y:.1f})")
        else:
            print("Rejected")
