object ZellersCongruence {
  private val weekdays = Array("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  private val monthNames = Array("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  private val monthDays = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  def isLeapYear(year: Int): Boolean = {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
  }

  def isValidDate(day: Int, month: Int, year: Int): Boolean = {
    if (month < 1 || month > 12 || day < 1 || year < 1583) return false
    val maxDays = if (month == 2 && isLeapYear(year)) 29 else monthDays(month - 1)
    day <= maxDays
  }

  def getZeller(day: Int, month: Int, year: Int): Int = {
    val (m, y) = if (month == 1 || month == 2) (month + 12, year - 1) else (month, year)
    val K = y % 100
    val J = y / 100
    (day + ((13 * (m + 1)) / 5) + K + (K / 4) + (J / 4) + 5 * J) % 7
  }

  def getWeekday(day: Int, month: Int, year: Int): String = {
    if (!isValidDate(day, month, year)) return "Invalid date"
    weekdays(getZeller(day, month, year))
  }

  def main(args: Array[String]): Unit = {
    val tests = List((1, 1, 2000), (1, 3, 2000), (18, 9, 1985), (12, 4, 1861))
    for ((day, month, year) <- tests) {
      println(s"${monthNames(month - 1)} $day, $year is a ${getWeekday(day, month, year)}")
    }
  }
}
