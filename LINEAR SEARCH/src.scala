object LinearSearch {
  def linearSearch(arr: Array[Int], target: Int): Int = {
    for (i <- arr.indices) {
      if (arr(i) == target) return i
    }
    -1
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(3, 7, 1, 9, 4)
    val target = 9
    val result = linearSearch(arr, target)
    println(s"Target $target found at index: $result")
  }
}
