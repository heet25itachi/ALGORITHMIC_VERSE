Point = Struct.new(:x, :y)

module CyrusBeck
  VERTICES = [Point.new(0, 0), Point.new(10, 0), Point.new(10, 10), Point.new(0, 10)]
  N_VERTICES = 4

  def self.compute_normal(i)
    v1, v2 = VERTICES[i], VERTICES[(i + 1) % N_VERTICES]
    Point.new(-(v2.y - v1.y), v2.x - v1.x)
  end

  def self.dot_product(a, b)
    a.x * b.x + a.y * b.y
  end

  def self.cyrus_beck_clip(p0, p1)
    D = Point.new(p1.x - p0.x, p1.y - p0.y)
    return [false, p0, p1] if D.x == 0 && D.y == 0

    tE, tL = 0.0, 1.0
    N_VERTICES.times do |i|
      normal = compute_normal(i)
      pe = VERTICES[i]
      diff = Point.new(p0.x - pe.x, p0.y - pe.y)
      num = -dot_product(normal, diff)
      den = dot_product(normal, D)
      next if den == 0
      t = num / den
      if den > 0
        tL = t if t < tL
      else
        tE = t if t > tE
      end
    end
    return [false, p0, p1] if tE > tL || tE < 0 || tE > 1 || tL < 0 || tL > 1

    p0_new = Point.new(p0.x + tE * D.x, p0.y + tE * D.y)
    p1_new = Point.new(p0.x + tL * D.x, p0.y + tL * D.y)
    [true, p0_new, p1_new]
  end
end

tests = [[2, 2, 8, 8], [12, 12, 15, 15], [5, 12, 15, 5], [-5, 5, 15, 5]]
tests.each do |test|
  p0 = Point.new(test[0], test[1])
  p1 = Point.new(test[2], test[3])
  print "Line from (#{p0.x}, #{p0.y}) to (#{p1.x}, #{p1.y}): "
  accept, p0_new, p1_new = CyrusBeck.cyrus_beck_clip(p0, p1)
  if accept
    puts "Accepted, clipped to (#{p0_new.x}, #{p0_new.y}) to (#{p1_new.x}, #{p1_new.y})"
  else
    puts "Rejected"
  end
end
