public class ZellersCongruence {
    private static final String[] weekdays = {"Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"};
    private static final String[] monthNames = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
    private static final int[] monthDays = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

    public static boolean isLeapYear(int year) {
        return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    }

    public static boolean isValidDate(int day, int month, int year) {
        if (month < 1 || month > 12 || day < 1 || year < 1583) return false;
        int maxDays = monthDays[month - 1];
        if (month == 2 && isLeapYear(year)) maxDays = 29;
        return day <= maxDays;
    }

    public static int getZeller(int day, int month, int year) {
        if (month == 1 || month == 2) {
            month += 12;
            year--;
        }
        int K = year % 100;
        int J = year / 100;
        return (day + ((13 * (month + 1)) / 5) + K + (K / 4) + (J / 4) + 5 * J) % 7;
    }

    public static String getWeekday(int day, int month, int year) {
        if (!isValidDate(day, month, year)) return "Invalid date";
        return weekdays[getZeller(day, month, year)];
    }

    public static void main(String[] args) {
        int[][] tests = {{1, 1, 2000}, {1, 3, 2000}, {18, 9, 1985}, {12, 4, 1861}};
        for (int[] test : tests) {
            int day = test[0], month = test[1], year = test[2];
            System.out.printf("%s %d, %d is a %s\n", monthNames[month - 1], day, year, getWeekday(day, month, year));
        }
    }
}
