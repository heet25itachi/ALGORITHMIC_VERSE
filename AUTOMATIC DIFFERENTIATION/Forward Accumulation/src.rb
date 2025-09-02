class Dual
  attr_accessor :value, :deriv
  def initialize(v, d)
    @value = v
    @deriv = d
  end
  def +(b)
    Dual.new(value + b.value, deriv + b.deriv)
  end
  def *(b)
    Dual.new(value * b.value, deriv * b.value + value * b.deriv)
  end
end

def sin(a)
  Dual.new(Math.sin(a.value), Math.cos(a.value) * a.deriv)
end

x = Dual.new(3.14, 1.0)
x2 = x * x
sx = sin(x)
f = x2 * sx
puts "f(x) = %.2f, f'(x) = %.2f" % [f.value, f.deriv]
