INSIDE, LEFT, RIGHT, BOTTOM, TOP = 0, 1, 2, 4, 8
XMIN, YMIN, XMAX, YMAX = 0.0, 0.0, 10.0, 10.0

def compute_outcode(x, y):
    code = INSIDE
    if x < XMIN:
        code |= LEFT
    elif x > XMAX:
        code |= RIGHT
    if y < YMIN:
        code |= BOTTOM
    elif y > YMAX:
        code |= TOP
    return code

def cohen_sutherland_clip(x0, y0, x1, y1):
    outcode0 = compute_outcode(x0, y0)
    outcode1 = compute_outcode(x1, y1)
    accept = False
    while True:
        if not (outcode0 | outcode1):
            accept = True
            break
        elif outcode0 & outcode1:
            break
        else:
            x, y = 0.0, 0.0
            outcode_out = outcode1 if outcode1 > outcode0 else outcode0
            if outcode_out & TOP:
                x = x0 + (x1 - x0) * (YMAX - y0) / (y1 - y0)
                y = YMAX
            elif outcode_out & BOTTOM:
                x = x0 + (x1 - x0) * (YMIN - y0) / (y1 - y0)
                y = YMIN
            elif outcode_out & RIGHT:
                y = y0 + (y1 - y0) * (XMAX - x0) / (x1 - x0)
                x = XMAX
            elif outcode_out & LEFT:
                y = y0 + (y1 - y0) * (XMIN - x0) / (x1 - x0)
                x = XMIN
            if outcode_out == outcode0:
                x0, y0 = x, y
                outcode0 = compute_outcode(x0, y0)
            else:
                x1, y1 = x, y
                outcode1 = compute_outcode(x1, y1)
    return accept, x0, y0, x1, y1

if __name__ == "__main__":
    tests = [(2, 2, 8, 8), (12, 12, 15, 15), (5, 12, 15, 5), (-5, 5, 15, 5)]
    for x0, y0, x1, y1 in tests:
        print(f"Line from ({x0:.1f}, {y0:.1f}) to ({x1:.1f}, {y1:.1f}): ", end="")
        accept, x0, y0, x1, y1 = cohen_sutherland_clip(x0, y0, x1, y1)
        if accept:
            print(f"Accepted, clipped to ({x0:.1f}, {y0:.1f}) to ({x1:.1f}, {y1:.1f})")
        else:
            print("Rejected")
