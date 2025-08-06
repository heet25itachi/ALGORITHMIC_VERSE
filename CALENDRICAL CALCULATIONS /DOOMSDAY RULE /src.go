package main

import "fmt"

var weekdays = []string{"Noneday", "Oneday", "Twosday", "Treblesday", "Foursday", "Fiveday", "Six-a-day"}
var doomsdayDates = [12][2]int{{3, 4}, {28, 29}, {14, 14}, {4, 4}, {9, 9}, {6, 6}, {11, 11}, {8, 8}, {5, 5}, {10, 10}, {7, 7}, {12, 12}}
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

func getDoomsday(year int) int {
    c := year / 100
    y := year % 100
    anchor := (5*(c%4) + 2) % 7
    a := y / 12
    b := y % 12
    c_y := b / 4
    return (anchor + a + b + c_y) % 7
}

func getWeekday(day, month, year int) string {
    if !isValidDate(day, month, year) {
        return "Invalid date"
    }
    doomsday := getDoomsday(year)
    refDay := doomsdayDates[month-1][0]
    if isLeapYear(year) && month <= 2 {
        refDay = doomsdayDates[month-1][1]
    }
    diff := (day - refDay) % 7
    if diff < 0 {
        diff += 7
    }
    return weekdays[(doomsday+diff)%7]
}

func main() {
    tests := [][3]int{{18, 9, 1985}, {12, 4, 1861}, {25, 12, 2021}, {7, 8, 1966}}
    for _, test := range tests {
        day, month, year := test[0], test[1], test[2]
        fmt.Printf("%s %d, %d is a %s\n", monthNames[month-1], day, year, getWeekday(day, month, year))
    }
}
