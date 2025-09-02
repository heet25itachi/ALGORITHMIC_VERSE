import static java.lang.Math.sin;
import static java.lang.Math.cos;

class Dual {
    double value, deriv;
    Dual(double v, double d) { value = v; deriv = d; }
    Dual add(Dual b) { return new Dual(value + b.value, deriv + b.deriv); }
    Dual mul(Dual b) { return new Dual(value * b.value, deriv * b.value + value * b.deriv); }
    Dual sin() { return new Dual(sin(value), cos(value) * deriv); }
}

public class AD {
    public static void main(String[] args) {
        Dual x = new Dual(3.14, 1.0);
        Dual x2 = x.mul(x);
        Dual sx = x.sin();
        Dual f = x2.mul(sx);
        System.out.printf("f(x) = %.2f, f'(x) = %.2f\n", f.value, f.deriv);
    }
}
