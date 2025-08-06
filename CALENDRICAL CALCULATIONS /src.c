#include <stdio.h>
#include <math.h>

int get_month_index(int month) {
    int indices[] = {11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}; // Jan=11, Feb=12, etc.
    return (month >= 1 && month <= 12) ? indices[month - 1] : -1;
}

const char* get_month_name(int month) {
    const char* names[] = {"January", "February", "March", "April", "May", "June",
                           "July", "August", "September", "October", "November", "December"};
    return (month >= 1 && month <= 12) ? names[month - 1] : "Invalid";
}

int days_in_month(int month, int year) {
    if (month < 1 || month > 12 || year < 1753) return -1;
    int m = get_month_index(month);
    double y = (double)year;

    double d = 30.0 + floor(0.6 * m + 0.4) - floor(0.6 * m - 0.2) - 2.0 * floor(m / 12.0);
    if (m == 12) { // February
        d += floor((y - 1) / 4.0 -
