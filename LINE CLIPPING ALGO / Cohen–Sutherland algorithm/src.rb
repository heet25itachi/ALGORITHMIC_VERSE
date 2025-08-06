module CohenSutherland
  INSIDE = 0
  LEFT = 1
  RIGHT = 2
  BOTTOM = 4
  TOP = 8

  XMIN = 0.0
  YMIN = 0.0
  XMAX = 10.0
  YMAX = 10.0

  def self.compute_outcode(x, y)
    code = INSIDE
    code |= LEFT if x < XMIN
    code |= RIGHT if x > XMAX
    code |= BOTTOM if y < YMIN
    code |= TOP if y > YMAX
    code
  end

  def self.cohen_sutherland_clip(x0, y0, x1, y1)
    x0, y0, x1, y1 = x0, y0, x1, y1
    outcode0 = compute_outcode(x0, y0)
    outcode1 = compute_outcode(x1, y1)
    accept = false

    loop do
      if (outcode0 | outcode1) == 0
        accept = true
        break
      elsif (outcode0 & outcode1) != 0
        break
      else
        x, y = 0.0, 0.0
        outcode_out = outcode1 > outcode0 ? outcode1 : outcode0
        if outcode_out & TOP != 0
          x = x0 + (x1 - x0) * (YMAX - y0) / (y1 - y0)
          y = YMAX
        elsif outcode_out & BOTTOM != 0
          x = x0 + (x1 - x0) * (YMIN - y0) / (y1 - y0)
          y = YMIN
        elsif outcode_out & RIGHT != 0
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
    [accept, x0, y0, x1, y1]
  end
end

tests = [[2, 2, 8, 8], [12, 12, 15, 15], [5, 12, 15, 5], [-5, 5, 15, 5]]
tests.each do |test|
  x0, y0, x1, y1 = test
  print "Line from (#{x0}, #{y0}) to (#{x1}, #{y1}): "
  accept, x0, y0, x1, y1 = CohenSutherland.cohen_sutherland_clip(x0, y0, x1, y1)
  if accept
    puts "Accepted, clipped to (#{x0}, #{y0}) to (#{x1}, #{y1})"
  else
    puts "Rejected"
  end
end
