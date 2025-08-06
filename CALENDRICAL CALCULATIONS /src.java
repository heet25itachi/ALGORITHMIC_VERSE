public class CalendricalCalculation {
    private static int getMonthIndex(int month) {
        int[] indices = {11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        return (month >= 1 && month <= 12) ? indices[month - 1] : -1;
    }

    private static String getMonthName(int month) {
        String[] names = {"January", "February", "March", "April", "May", "June",
                         "July", "August", "September", "October", "November", "December"};
        return (month >= 1 && month <= 12) ? names[month - 1] : "Invalid";
    }

    public static int daysInMonth(int month, int year) {
        if (month < 1 || month > 12 || year < 1753) return -1;
        int m = getMonthIndex(month);
        double y = year;

        double d = 30.0 + Math.floor(0.6 * m + 0.4) - Math.floor(0.6 * m - 0.2) - 2.0 * Math.floor(m / 12.0);
        if (m == 12) {
            d += Math.floor((y - 1) / 4.0 - Math.floor((y - 1) / 4.0) + 0.25);
            if (year % 100 == 0) {
                double centuryTerm = Math.floor(0.3 + (Math.floor(y / 100.0) - 3) / 4.5 - Math.floor((Math.floor(y / 100.0) - 3) / 4.5));
                d += Math.floor((centuryTerm + 99.0 + 100.0 * (y / 100.0 - Math.floor(y / 100.0))) / 100.0) - 1.0;
            }
        }
        return (int)d;
    }

    public static void main(String[] args) {
        int[][] tests = {{2, 2000}, {3, 2023}, {4, 2024}, {2, 1900}};
        for (int[] test : tests) {
            int month = test[0], year = test[1];
            int days = daysInMonth(month, year);
            if (days != -1)
                System.out.printf("Days in %s %d: %d\n", getMonthName(month), year, days);
            else
                System.out.println("Invalid input");
        }
    }
}
