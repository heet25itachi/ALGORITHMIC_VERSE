import java.util.HashMap;
import java.util.Random;

public class BirthdayAttack {
    static final int R = 65536;

    static void birthdayAttack() {
        HashMap<Integer, Long> table = new HashMap<>();
        Random rng = new Random();
        int trials = 0;
        while (true) {
            long x = rng.nextLong();
            int h = (int) (x % R);
            trials++;
            if (table.containsKey(h) && !table.get(h).equals(x)) {
                System.out.printf("Collision found after %d trials: x_i = %d, x_j = %d, hash = %d\n",
                        trials, table.get(h), x, h);
                break;
            }
            table.put(h, x);
        }
    }

    public static void main(String[] args) {
        birthdayAttack();
    }
}
