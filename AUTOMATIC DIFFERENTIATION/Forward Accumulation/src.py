import math

class Dual:
    def __init__(self, value, deriv):
        self.value = value
        self.deriv = deriv

    def __add__(self, b):
        return Dual(self.value + b.value, self.deriv + b.deriv)

    def __mul__(self, b):
        return Dual(self.value * b.value, self.deriv * b.value + self.value * b.deriv)

def sin(a):
    return Dual(math.sin(a.value), math.cos(a.value) * a.deriv)

x = Dual(3.14, 1.0)
x2 = x * x
sx = sin(x)
f = x2 * sx
print(f"f(x) = {f.value:.2f}, f'(x) = {f.deriv:.2f}")
