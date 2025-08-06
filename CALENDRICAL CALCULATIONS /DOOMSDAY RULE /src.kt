object DoomsdayRule {
    private val weekdays = arrayOf("Noneday", "Oneday", "Twosday", "Treblesday", "Foursday", "Fiveday", "Six-a-day")
    private val doomsdayDates = arrayOf(intArrayOf(3, 4), intArrayOf(28, 29), intArrayOf(14, 14), intArrayOf(4, 4), intArrayOf(9, 9), intArrayOf(6, 6),
                                       intArrayOf(11, 11), intArrayOf(8, 8), intArrayOf(5, 5), intArrayOf(10, 10), intArrayOf(7, 7), intArrayOf(12, 12))
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

    fun getDoomsday(year: Int): Int {
        val c = year / 100
        val y = year % 100
        val anchor = (5 * (c % 4) + 2) % 7
        val a = y / 12
        val b = y % 12
        val c_y = b / 4
        return (anchor + a + b + c_y) % 7
    }

    fun getWeekday(day: Int, month: Int, year: Int): String {
        if (!isValidDate(day, month, year)) return "Invalid date"
        val doomsday = getDoomsday(year)
        val refDay = doomsdayDates[month - 1][if (isLeapYear(year) && month <= 2) 1 else 0]
        var diff = (day - refDay) % 7
        if (diff < 0) diff += 7
        return weekdays[(doomsday + diff) % 7]
    }
}

fun main() {
    val tests = listOf(Triple(18, 9, 1985), Triple(12, 4, 1861), Triple(25, 12, 2021), Triple(7, 8, 1966))
    for ((day, month, year) in tests) {
        println("${DoomsdayRule.monthNames[month - 1]} $day, $year is a ${DoomsdayRule.getWeekday(day, month, year)}")
    }
}
