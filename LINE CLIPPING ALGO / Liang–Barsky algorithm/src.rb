Point = Struct.new(:x, :y)

module LiangBarsky
  def self.liang_barsky_clip(xmin, ymin, xmax, ymax, p0, p1)
    return [false, p0, p1] if xmin >= xmax || ymin >= ymax

    dx = p1.x - p0.x
    dy = p1.y - p0.y
    p = [-dx, dx, -dy, dy]
    q = [p0.x - xmin, xmax - p0.x, p0.y - ymin, ymax - p0.y]
    u1, u2 = 0.0, 1.0

    4.times do |i|
      return [false, p0, p1] if p[i] == 0 && q[i] < 0
      if p[i] != 0
        t = q[i] / p[i]
        if p[i] < 0
          u1 = t if t > u1
        else
          u2 = t if t < u2
        end
      end
    end
    return [false, p0, p1] if u1 > u2

    p0_new = Point.new(p0.x + u1 * dx, p0.y + u1 * dy)
    p1_new = Point.new(p0.x + u2 * dx, p0.y + u2 * dy)
    [true, p0_new, p1_new]
  end
end

xmin, ymin, xmax, ymax = 0, 0, 10, 10
tests = [[2, 2, 8, 8], [12, 12, 15, 15], [5, 12, 15, 5], [-5, 5, 15, 5]]
tests.each do |test|
  p0 = Point.new(test[0], test[1])
  p1 = Point.new(test[2], test[3])
  print "Line from (#{p0.x}, #{p0.y}) to (#{p1.x}, #{p1.y}): "
  accept, p0_new, p1_new = LiangBarsky.liang_barsky_clip(xmin, ymin, xmax, ymax, p0, p1)
  if accept
    puts "Accepted, clipped to (#{p0_new.x}, #{p0_new.y}) to (#{p1_new.x}, #{p1_new.y})"
  else
    puts "Rejected"
  end
end
