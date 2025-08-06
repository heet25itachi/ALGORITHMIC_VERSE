const INSIDE = 0
const LEFT = 1
const RIGHT = 2
const BOTTOM = 4
const TOP = 8

const XMIN = 0.0
const YMIN = 0.0
const XMAX = 10.0
const YMAX = 10.0

function compute_outcode(x::Float64, y::Float64)::Int
    code = INSIDE
    if x < XMIN
        code |= LEFT
    elseif x > XMAX
        code |= RIGHT
    end
    if y < YMIN
        code |= BOTTOM
    elseif y > YMAX
        code |= TOP
    end
    code
end

function cohen_sutherland_clip(x0::Float64, y0::Float64, x1::Float64, y1::Float64)
    x0, y0, x1, y1 = x0, y0, x1, y1
    outcode0 = compute_outcode(x0, y0)
    outcode1 = compute_outcode(x1, y1)
    accept = false

    while true
        if (outcode0 | outcode1) == 0
            accept = true
            break
        elseif (outcode0 & outcode1) != 0
            break
        else
            x, y = 0.0, 0.0
            outcode_out = outcode1 > outcode0 ? outcode1 : outcode0
            if (outcode_out & TOP) != 0
                x = x0 + (x1 - x0) * (YMAX - y0) / (y1 - y0)
                y = YMAX
            elseif (outcode_out & BOTTOM) != 0
                x = x0 + (x1 - x0) * (YMIN - y0) / (y1 - y0)
                y = YMIN
            elseif (outcode_out & RIGHT) != 0
                y = y0 + (y1 - y0) * (XMAX - x0) / (x1 - x0)
                x = XMAX
            else
                y = y0 + (y1 - y0) * (XMIN - x0) / (x1 - x0)
                x = XMIN
            end
            if outcode_out == outcode0
                x0, y0 = x, y
                outcode0 = compute_outcode(x0, y0)
            else
                x1, y1 = x, y
                outcode1 = compute_outcode(x1, y1)
            end
        end
    end
    (accept, x0, y0, x1, y1)
end

tests = [(2.0, 2.0, 8.0, 8.0), (12.0, 12.0, 15.0, 15.0), (5.0, 12.0, 15.0, 5.0), (-5.0, 5.0, 15.0, 5.0)]
for (x0, y0, x1, y1) in tests
    print("Line from ($x0, $y0) to ($x1, $y1): ")
    accept, x0_new, y0_new, x1_new, y1_new = cohen_sutherland_clip(x0, y0, x1, y1)
    if accept
        println("Accepted, clipped to ($x0_new, $y0_new) to ($x1_new, $y1_new)")
    else
        println("Rejected")
    end
end
