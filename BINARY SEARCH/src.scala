object BinarySearch {
  def binarySearch(arr: Array[Int], target: Int): Int = {
    var left = 0
    var right = arr.length - 1
    while (left <= right) {
      val mid = (left + right) / 2
      if (arr(mid) == target) mid
      else if (arr(mid) < target) left = mid + 1
      else right = mid - 1
    }
    -1
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(1, 3, 4, 7, 9)
    val target = 9
    val result = binarySearch(arr, target)
    println(s"Target $target found at index: $result")
  }
}
