#include <iostream>

double y(double k, int steps) {
    double result = 1.0; // y(0) = 1
    for (int i = 0; i < steps; i++) {
        result *= i; // y(k+1) = k * y(k)
    }
    return result;
}

int main() {
    int k = 5;
    std::cout << "Numerical solution for y(" << k << ") = " << y(k, k) << std::endl;
    return 0;
}
