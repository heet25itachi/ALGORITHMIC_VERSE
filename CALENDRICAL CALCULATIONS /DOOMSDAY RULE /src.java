public class DoomsdayRule {
    private static final String[] weekdays = {"Noneday", "Oneday", "Twosday", "Treblesday", "Foursday", "Fiveday", "Six-a-day"};
    private static final int[][] doomsdayDates = {{3, 4}, {28, 29}, {14, 14}, {4, 4}, {9, 9}, {6, 6}, {11, 11}, {8, 8}, {5, 5}, {10, 10}, {7, 7}, {12, 12}};
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

    public static int getDoomsday(int year) {
        int c = year / 100;
        int y = year % 100;
        int anchor = (5 * (c % 4) + 2) % 7;
        int a = y / 12;
        int b = y % 12;
        int c_y = b / 4;
        return (anchor + a + b + c_y) % 7;
    }

    public static String getWeekday(int day, int month, int year) {
        if (!isValidDate(day, month, year)) return "Invalid date";
        int doomsday = getDoomsday(year);
        int refDay = doomsdayDates[month - 1][isLeapYear(year) && month <= 2 ? 1 : 0];
        int diff = (day - refDay) % 7;
        if (diff < 0) diff += 7;
        return weekdays[(doomsday + diff) % 7];
    }

    public static void main(String[] args) {
        int[][] tests = {{18, 9, 1985}, {12, 4, 1861}, {25, 12, 2021}, {7, 8, 1966}};
        for (int[] test : tests) {
            int day = test[0], month = test[1], year = test[2];
            System.out.printf("%s %d, %d is a %s\n", monthNames[month - 1], day, year, getWeekday(day, month, year));
        }
    }
}
