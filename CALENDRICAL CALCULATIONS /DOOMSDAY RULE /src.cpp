#include <iostream>
#include <string>
#include <vector>
using namespace std;

const vector<string> weekdays = {"Noneday", "Oneday", "Twosday", "Treblesday", "Foursday", "Fiveday", "Six-a-day"};
const int doomsday_dates[12][2] = {{3, 4}, {28, 29}, {14, 14}, {4, 4}, {9, 9}, {6, 6}, {11, 11}, {8, 8}, {5, 5}, {10, 10}, {7, 7}, {12, 12}};
const vector<string> month_names = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
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

int get_doomsday(int year) {
    int c = year / 100;
    int y = year % 100;
    int anchor = (5 * (c % 4) + 2) % 7;
    int a = y / 12;
    int b = y % 12;
    int c_y = b / 4;
    return (anchor + a + b + c_y) % 7;
}

string get_weekday(int day, int month, int year) {
    if (!is_valid_date(day, month, year)) return "Invalid date";
    int doomsday = get_doomsday(year);
    int ref_day = doomsday_dates[month - 1][is_leap_year(year) && month <= 2 ? 1 : 0];
    int diff = (day - ref_day) % 7;
    if (diff < 0) diff += 7;
    return weekdays[(doomsday + diff) % 7];
}

int main() {
    vector<vector<int>> tests = {{18, 9, 1985}, {12, 4, 1861}, {25, 12, 2021}, {7, 8, 1966}};
    for (const auto& test : tests) {
        int day = test[0], month = test[1], year = test[2];
        cout << month_names[month - 1] << " " << day << ", " << year << " is a " << get_weekday(day, month, year) << endl;
    }
    return 0;
}
