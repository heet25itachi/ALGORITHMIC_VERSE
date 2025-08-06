object CalendricalCalculation {
    private val indices = listOf(11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    private val names = listOf("January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November", "December")

    fun getMonthIndex(month: Int): Int {
        return if (month in 1..12) indices[month - 1] else -1
    }

    fun getMonthName(month: Int): String {
        return if (month in 1..12) names[month - 1] else "Invalid"
    }

    fun daysInMonth(month: Int, year: Int): Int {
        if (month !in 1..12 || year < 1753) return -1
        val m = getMonthIndex(month).toDouble()
        val y = year.toDouble()

        var d = 30.0 + Math.floor(0.6 * m + 0.4) - Math.floor(0.6 * m - 0.2) - 2.0 * Math.floor(m / 12.0)
        if (m == 12.0) {
            d += Math.floor((y - 1) / 4.0 - Math.floor((y - 1) / 4.0) + 0.25)
            if (year % 100 == 0) {
                val centuryTerm = Math.floor(0.3 + (Math.floor(y / 100.0) - 3) / 4.5 - Math.floor((Math.floor(y / 100.0) - 3) / 4.5))
                d += Math.floor((centuryTerm + 99.0 + 100.0 * (y / 100.0 - Math.floor(y / 100.0))) / 100.0) - 1.0
            }
        }
        return d.toInt()
    }
}

fun main() {
    val tests = listOf(Pair(2, 2000), Pair(3, 2023), Pair(4, 2024), Pair(2, 1900))
    for ((month, year) in tests) {
        val days = CalendricalCalculation.daysInMonth(month, year)
        if (days != -1)
            println("Days in ${CalendricalCalculation.getMonthName(month)} $year: $days")
        else
            println("Invalid input")
    }
}
