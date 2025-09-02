struct Dual
    value::Float64
    deriv::Float64
end

import Base: +, *
+(a::Dual, b::Dual) = Dual(a.value + b.value, a.deriv + b.deriv)
*(a::Dual, b::Dual) = Dual(a.value * b.value, a.deriv * b.value + a.value * b.deriv)

sin(a::Dual) = Dual(sin(a.value), cos(a.value) * a.deriv)

x = Dual(3.14, 1.0)
x2 = x * x
sx = sin(x)
f = x2 * sx
println("f(x) = ", round(f.value, digits=2), ", f'(x) = ", round(f.deriv, digits=2))
