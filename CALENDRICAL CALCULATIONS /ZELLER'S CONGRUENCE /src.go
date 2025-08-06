package main

import "fmt"

var weekdays = []string{"Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"}
var monthNames = []string{"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"}
var monthDays = []int{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}

func isLeapYear(year int) bool {
    return year%4 == 0 && (year%100 != 0 || year%400 == 0)
}

func isValidDate(day, month, year int) bool {
    if month < 1 || month > 12 || day < 1 || year < 1583 {
        return false
    }
    maxDays := monthDays[month-1]
    if month == 2 && isLeapYear(year) {
        maxDays = 29
    }
    return day <= maxDays
}

func getZeller(day, month, year int) int {
    if month == 1 || month == 2 {
        month += 12
        year--
    }
    K := year % 100
    J := year / 100
    return (day + ((13 * (month + 1)) / 5) + K + (K / 4) + (J / 4) + 5*J) % 7
}

func getWeekday(day, month, year int) string {
    if !isValidDate(day, month, year) {
        return "Invalid date"
    }
    return weekdays[getZeller(day, month, year)]
}

func main() {
    tests := [][3]int{{1, 1, 2000}, {1, 3, 2000}, {18, 9, 1985}, {12, 4, 1861}}
    for _, test := range tests {
        day, month, year := test[0], test[1], test[2]
        fmt.Printf("%s %d, %d is a %s\n", monthNames[month-1], day, year, getWeekday(day, month, year))
    }
} 
