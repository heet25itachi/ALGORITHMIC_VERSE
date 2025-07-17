public class Difference {
    public static double y(double k, int steps) {
        double result = 1.0; // y(0) = 1
        for (int i = 0; i < steps; i++) {
            result *= i; // y(k+1) = k * y(k)
        }
        return result;
    }

    public static void main(String[] args) {
        int k = 5;
        System.out.println("Numerical solution for y(" + k + ") = " + y(k, k));
    }
}
