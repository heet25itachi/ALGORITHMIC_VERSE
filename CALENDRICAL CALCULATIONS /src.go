package main

import (
    "fmt"
    "math"
)

func getMonthIndex(month int) int {
    indices := []int{11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    if month >= 1 && month <= 12 {
        return indices[month-1]
    }
    return -1
}

func getMonthName(month int) string {
    names := []string{"January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"}
    if month >= 1 && month <= 12 {
        return names[month-1]
    }
    return "Invalid"
}

func daysInMonth(month, year int) int {
    if month < 1 || month > 12 || year < 1753 {
        return -1
    }
    m := float64(getMonthIndex(month))
    y := float64(year)

    d := 30.0 + math.Floor(0.6*m+0.4) - math.Floor(0.6*m-0.2) - 2.0*math.Floor(m/12.0)
    if m == 12 {
        d += math.Floor((y-1)/4.0 - math.Floor((y-1)/4.0) + 0.25)
        if year%100 == 0 {
            centuryTerm := math.Floor(0.3 + (math.Floor(y/100.0)-3)/4.5 - math.Floor((math.Floor(y/100.0)-3)/4.5))
            d += math.Floor((centuryTerm + 99.0 + 100.0*(y/100.0-math.Floor(y/100.0)))/100.0) - 1.0
        }
    }
    return int(d)
}

func main() {
    tests := [][2]int{{2, 2000}, {3, 2023}, {4, 2024}, {2, 1900}}
    for _, test := range tests {
        month, year := test[0], test[1]
        days := daysInMonth(month, year)
        if days != -1 {
            fmt.Printf("Days in %s %d: %d\n", getMonthName(month), year, days)
        } else {
            fmt.Println("Invalid input")
        }
    }
}
