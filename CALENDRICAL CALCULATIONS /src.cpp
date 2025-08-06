#include <iostream>
#include <cmath>
using namespace std;

int get_month_index(int month) {
    int indices[] = {11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    return (month >= 1 && month <= 12) ? indices[month - 1] : -1;
}

string get_month_name(int month) {
    string names[] = {"January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November", "December"};
    return (month >= 1 && month <= 12) ? names[month - 1] : "Invalid";
}

int days_in_month(int month, int year) {
    if (month < 1 || month > 12 || year < 1753) return -1;
    int m = get_month_index(month);
    double y = year;

    double d = 30.0 + floor(0.6 * m + 0.4) - floor(0.6 * m - 0.2) - 2.0 * floor(m / 12.0);
    if (m == 12) {
        d += floor((y - 1) / 4.0 - floor((y - 1) / 4.0) + 0.25);
        if (year % 100 == 0) {
            double century_term = floor(0.3 + (floor(y / 100.0) - 3) / 4.5 - floor((floor(y / 100.0) - 3) / 4.5));
            d += floor((century_term + 99.0 + 100.0 * (y / 100.0 - floor(y / 100.0))) / 100.0) - 1.0;
        }
    }
    return (int)d;
}

int main() {
    int tests[][2] = {{2, 2000}, {3, 2023}, {4, 2024}, {2, 1900}};
    for (int i = 0; i < 4; i++) {
        int month = tests[i][0], year = tests[i][1];
        int days = days_in_month(month, year);
        if (days != -1)
            cout << "Days in " << get_month_name(month) << " " << year << ": " << days << endl;
        else
            cout << "Invalid input" << endl;
    }
    return 0;
}
