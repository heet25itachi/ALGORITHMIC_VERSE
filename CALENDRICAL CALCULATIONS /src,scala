object CalendricalCalculation {
  private val indices = Array(11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  private val names = Array("January", "February", "March", "April", "May", "June",
                           "July", "August", "September", "October", "November", "December")

  def getMonthIndex(month: Int): Int = {
    if (month >= 1 && month <= 12) indices(month - 1) else -1
  }

  def getMonthName(month: Int): String = {
    if (month >= 1 && month <= 12) names(month - 1) else "Invalid"
  }

  def daysInMonth(month: Int, year: Int): Int = {
    if (month < 1 || month > 12 || year < 1753) return -1
    val m = getMonthIndex(month).toDouble
    val y = year.toDouble

    var d = 30.0 + math.floor(0.6 * m + 0.4) - math.floor(0.6 * m - 0.2) - 2.0 * math.floor(m / 12.0)
    if (m == 12) {
      d += math.floor((y - 1) / 4.0 - math.floor((y - 1) / 4.0) + 0.25)
      if (year % 100 == 0) {
        val centuryTerm = math.floor(0.3 + (math.floor(y / 100.0) - 3) / 4.5 - math.floor((math.floor(y / 100.0) - 3) / 4.5))
        d += math.floor((centuryTerm + 99.0 + 100.0 * (y / 100.0 - math.floor(y / 100.0))) / 100.0) - 1.0
      }
    }
    d.toInt
  }

  def main(args: Array[String]): Unit = {
    val tests = List((2, 2000), (3, 2023), (4, 2024), (2, 1900))
    for ((month, year) <- tests) {
      val days = daysInMonth(month, year)
      if (days != -1)
        println(s"Days in ${getMonthName(month)} $year: $days")
      else
        println("Invalid input")
    }
  }
}
