#include <stdio.h>

double y(double k, int steps) {
    double result = 1.0; // Initial condition y(0) = 1
    for (int i = 0; i < steps; i++) {
        result *= i; // y(k+1) = k * y(k)
    }
    return result;
}

int main() {
    int k = 5; // Example value
    printf("Numerical solution for y(%d) = %f\n", k, y(k, k));
    return 0;
}
