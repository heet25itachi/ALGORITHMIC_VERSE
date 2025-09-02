#include <stdio.h>
#include <math.h>

typedef struct {
    double value;
    double deriv;
} Dual;

Dual dual_add(Dual a, Dual b) {
    Dual res;
    res.value = a.value + b.value;
    res.deriv = a.deriv + b.deriv;
    return res;
}

Dual dual_mul(Dual a, Dual b) {
    Dual res;
    res.value = a.value * b.value;
    res.deriv = a.deriv * b.value + a.value * b.deriv;
    return res;
}

Dual dual_sin(Dual a) {
    Dual res;
    res.value = sin(a.value);
    res.deriv = cos(a.value) * a.deriv;
    return res;
}

int main() {
    Dual x = {3.14, 1.0}; // x with derivative 1
    Dual x2 = dual_mul(x, x);
    Dual sx = dual_sin(x);
    Dual f = dual_mul(x2, sx);
    printf("f(x) = %.2f, f'(x) = %.2f\n", f.value, f.deriv);
    return 0;
}
