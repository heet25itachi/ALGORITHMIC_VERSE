struct Point
    x::Float64
    y::Float64
end

const vertices = [Point(0, 0), Point(10, 0), Point(10, 10), Point(0, 10)]
const n_vertices = 4

function compute_normal(i::Int)
    v1, v2 = vertices[i+1], vertices[mod1(i+2, n_vertices)]
    Point(-(v2.y - v1.y), v2.x - v1.x)
end

function dot_product(a::Point, b::Point)
    a.x * b.x + a.y * b.y
end

function cyrus_beck_clip(p0::Point, p1::Point)
    D = Point(p1.x - p0.x, p1.y - p0.y)
    if D.x == 0 && D.y == 0
        return false, p0, p1
    end

    tE, tL = 0.0, 1.0
    for i in 0:n_vertices-1
        normal = compute_normal(i)
        PE = vertices[i+1]
        diff = Point(p0.x - PE.x, p0.y - PE.y)
        num = -dot_product(normal, diff)
        den = dot_product(normal, D)
        if den == 0
            continue
        end
        t = num / den
        if den > 0
            if t < tL
                tL = t
            end
        else
            if t > tE
                tE = t
            end
        end
    end
    if tE > tL || tE < 0 || tE > 1 || tL < 0 || tL > 1
        return false, p0, p1
    end

    p0_new = Point(p0.x + tE * D.x, p0.y + tE * D.y)
    p1_new = Point(p0.x + tL * D.x, p0.y + tL * D.y)
    return true, p0_new, p1_new
end

tests = [(2.0, 2.0, 8.0, 8.0), (12.0, 12.0, 15.0, 15.0), (5.0, 12.0, 15.0, 5.0), (-5.0, 5.0, 5.0)]
for (x0, y0, x1, y1) in tests
    p0, p1 = Point(x0, y0), Point(x1, y1)
    print("Line from ($x0, $y0) to ($x1, $y1): ")
    accept, p0_new, p1_new = cyrus_beck_clip(p0, p1)
    if accept
        println("Accepted, clipped to ($(p0_new.x), $(p0_new.y)) to ($(p1_new.x), $(p1_new.y))")
    else
        println("Rejected")
    end
end
