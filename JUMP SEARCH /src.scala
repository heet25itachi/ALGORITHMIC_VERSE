object JumpSearch {
  def jumpSearch(arr: Array[Int], target: Int): Int = {
    val size = arr.length
    val step = math.sqrt(size.toDouble).toInt
    var prev = 0
    while (arr(math.min(step, size) - 1) < target) {
      prev = step
      step += math.sqrt(size.toDouble).toInt
      if (prev >= size) return -1
    }
    while (prev < size && arr(prev) < target) {
      prev += 1
    }
    if (prev < size && arr(prev) == target) prev else -1
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(1, 3, 4, 7, 9)
    val target = 9
    val result = jumpSearch(arr, target)
    println(s"Target $target found at index: $result")
  }
}
