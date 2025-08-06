object DoomsdayRule {
  private val weekdays = Array("Noneday", "Oneday", "Twosday", "Treblesday", "Foursday", "Fiveday", "Six-a-day")
  private val doomsdayDates = Array(Array(3, 4), Array(28, 29), Array(14, 14), Array(4, 4), Array(9, 9), Array(6, 6),
                                   Array(11, 11), Array(8, 8), Array(5, 5), Array(10, 10), Array(7, 7), Array(12, 12))
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

  def getDoomsday(year: Int): Int = {
    val c = year / 100
    val y = year % 100
    val anchor = (5 * (c % 4) + 2) % 7
    val a = y / 12
    val b = y % 12
    val c_y = b / 4
    (anchor + a + b + c_y) % 7
  }

  def getWeekday(day: Int, month: Int, year: Int): String = {
    if (!isValidDate(day, month, year)) return "Invalid date"
    val doomsday = getDoomsday(year)
    val refDay = doomsdayDates(month - 1)(if (isLeapYear(year) && month <= 2) 1 else 0)
    var diff = (day - refDay) % 7
    if (diff < 0) diff += 7
    weekdays((doomsday + diff) % 7)
  }

  def main(args: Array[String]): Unit = {
    val tests = List((18, 9, 1985), (12, 4, 1861), (25, 12, 2021), (7, 8, 1966))
    for ((day, month, year) <- tests) {
      println(s"${monthNames(month - 1)} $day, $year is a ${getWeekday(day, month, year)}")
    }
  }
}
