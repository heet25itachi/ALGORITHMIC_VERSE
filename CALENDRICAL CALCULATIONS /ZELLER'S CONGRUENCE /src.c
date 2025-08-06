#include <stdio.h>
#include <stdbool.h>
#include <math.h>

const char* weekdays[] = {"Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"};
const char* month_names[] = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
const int month_days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

bool is_leap_year(int year) {
    return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
}

bool is_valid_date(int day, int month, int year) {
    if (month < 1 || month > 12 || day < 1 || year < 1583) return false;
    int max_days = month_days[month - 1];
    if (month == 2 && is_leap_year(year)) max_days = 29;
    return day <= max_days;
}

int get_zeller(int day, int month, int year) {
    if (month == 1 || month == 2) {
        month += 12;
        year--;
    }
    int K = year % 100;
    int J = year / 100;
    return (day + ((13 * (month + 1)) / 5) + K + (K / 4) + (J / 4) + 5 * J) % 7;
}

const char* get_weekday(int day, int month, int year) {
    if (!is_valid_date(day, month, year)) return "Invalid date";
    return weekdays[get_zeller(day, month, year)];
}

int main() {
    int tests[][3] = {{1, 1, 2000}, {1, 3, 2000}, {18, 9, 1985}, {12, 4, 1861}};
    for (int i = 0; i < 4; i++) {
        int day = tests[i][0], month = tests[i][1], year = tests[i][2];
        printf("%s %d, %d is a %s\n", month_names[month - 1], day, year, get_weekday(day, month, year));
    }
    return 0;
}
