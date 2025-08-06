class Point:
    def __init__(self, x, y):
        self.x, self.y = x, y

vertices = [Point(0, 0), Point(10, 0), Point(10, 10), Point(0, 10)]
n_vertices = 4

def compute_normal(i):
    v1, v2 = vertices[i], vertices[(i + 1) % n_vertices]
    return Point(-(v2.y - v1.y), v2.x - v1.x)

def dot_product(a, b):
    return a.x * b.x + a.y * b.y

def cyrus_beck_clip(p0, p1):
    D = Point(p1.x - p0.x, p1.y - p0.y)
    if D.x == 0 and D.y == 0:
        return False, p0, p1

    tE, tL = 0.0, 1.0
    for i in range(n_vertices):
        normal = compute_normal(i)
        PE = vertices[i]
        diff = Point(p0.x - PE.x, p0.y - PE.y)
        num = -dot_product(normal, diff)
        den = dot_product(normal, D)
        if den == 0:
            continue
        t = num / den
        if den > 0:
            if t < tL:
                tL = t
        else:
            if t > tE:
                tE = t
    if tE > tL or tE < 0 or tE > 1 or tL < 0 or tL > 1:
        return False, p0, p1

    p0_new = Point(p0.x + tE * D.x, p0.y + tE * D.y)
    p1_new = Point(p0.x + tL * D.x, p0.y + tL * D.y)
    return True, p0_new, p1_new

if __name__ == "__main__":
    tests = [(2, 2, 8, 8), (12, 12, 15, 15), (5, 12, 15, 5), (-5, 5, 15, 5)]
    for x0, y0, x1, y1 in tests:
        p0, p1 = Point(x0, y0), Point(x1, y1)
        print(f"Line from ({x0:.1f}, {y0:.1f}) to ({x1:.1f}, {y1:.1f}): ", end="")
        accept, p0_new, p1_new = cyrus_beck_clip(p0, p1)
        if accept:
            print(f"Accepted, clipped to ({p0_new.x:.1f}, {p0_new.y:.1f}) to ({p1_new.x:.1f}, {p1_new.y:.1f})")
        else:
            print("Rejected")
