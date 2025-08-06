object ZellersCongruence {
    private val weekdays = arrayOf("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    private val monthNames = arrayOf("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    private val monthDays = intArrayOf(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    fun isLeapYear(year: Int): Boolean {
        return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
    }

    fun isValidDate(day: Int, month: Int, year: Int): Boolean {
        if (month < 1 || month > 12 || day < 1 || year < 1583) return false
        val maxDays = if (month == 2 && isLeapYear(year)) 29 else monthDays[month - 1]
        return day <= maxDays
    }

    fun getZeller(day: Int, month: Int, year: Int): Int {
        var m = month
        var y = year
        if (m == 1 || m == 2) {
            m += 12
            y--
        }
        val K = y % 100
        val J = y / 100
        return (day + ((13 * (m + 1)) / 5) + K + (K / 4) + (J / 4) + 5 * J) % 7
    }

    fun getWeekday(day: Int, month: Int, year: Int): String {
        if (!isValidDate(day, month, year)) return "Invalid date"
        return weekdays[getZeller(day, month, year)]
    }
}

fun main() {
    val tests = listOf(Triple(1, 1, 2000), Triple(1, 3, 2000), Triple(18, 9, 1985), Triple(12, 4, 1861))
    for ((day, month, year) in tests) {
        println("${ZellersCongruence.monthNames[month - 1]} $day, $year is a ${ZellersCongruence.getWeekday(day, month, year)}")
    }
}
