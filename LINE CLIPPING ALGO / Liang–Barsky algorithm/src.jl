struct Point
    x::Float64
    y::Float64
end

function liang_barsky_clip(xmin, ymin, xmax, ymax, p0, p1)
    if xmin >= xmax || ymin >= ymax
        return false, p0, p1
    end

    dx = p1.x - p0.x
    dy = p1.y - p0.y
    p = [-dx, dx, -dy, dy]
    q = [p0.x - xmin, xmax - p0.x, p0.y - ymin, ymax - p0.y]
    u1, u2 = 0.0, 1.0

    for i in 1:4
        if p[i] == 0 && q[i] < 0
            return false, p0, p1
        end
        if p[i] != 0
            t = q[i] / p[i]
            if p[i] < 0
                if t > u1
                    u1 = t
                end
            else
                if t < u2
                    u2 = t
                end
            end
        end
    end
    if u1 > u2
        return false, p0, p1
    end

    p0_new = Point(p0.x + u1 * dx, p0.y + u1 * dy)
    p1_new = Point(p0.x + u2 * dx, p0.y + u2 * dy)
    return true, p0_new, p1_new
end

xmin, ymin, xmax, ymax = 0.0, 0.0, 10.0, 10.0
tests = [(2.0, 2.0, 8.0, 8.0), (12.0, 12.0, 15.0, 15.0), (5.0, 12.0, 15.0, 5.0), (-5.0, 5.0, 15.0, 5.0)]
for (x0, y0, x1, y1) in tests
    p0, p1 = Point(x0, y0), Point(x1, y1)
    print("Line from ($x0, $y0) to ($x1, $y1): ")
    accept, p0_new, p1_new = liang_barsky_clip(xmin, ymin, xmax, ymax, p0, p1)
    if accept
        println("Accepted, clipped to ($(p0_new.x), $(p0_new.y)) to ($(p1_new.x), $(p1_new.y))")
    else
        println("Rejected")
    end
end
