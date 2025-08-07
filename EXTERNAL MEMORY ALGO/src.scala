object ExternalMergeSort {
  def quicksort(arr: Array[Int], left: Int, right: Int): Unit = {
    if (left >= right) return
    val pivot = arr(right)
    var i = left
    var j = right
    while (i < j) {
      while (i < j && arr(i) <= pivot) i += 1
      while (i < j && arr(j) > pivot) j -= 1
      if (i < j) {
        val tmp = arr(i); arr(i) = arr(j); arr(j) = tmp
      }
    }
    arr(i) = arr(right); arr(right) = arr(i)
    quicksort(arr, left, i - 1)
    quicksort(arr, i + 1, right)
  }

  def mergeRuns(run1: Array[Int], run2: Array[Int], B: Int): Array[Int] = {
    val output = new Array[Int](run1.length + run2.length)
    var i, j, k = 0
    while (i < run1.length && j < run2.length) {
      for (_ <- 0 until B) {
        if (i < run1.length && (j >= run2.length || run1(i) <= run2(j))) {
          output(k) = run1(i); i += 1; k += 1
        } else if (j < run2.length) {
          output(k) = run2(j); j += 1; k += 1
        }
      }
    }
    while (i < run1.length) { output(k) = run1(i); i += 1; k += 1 }
    while (j < run2.length) { output(k) = run2(j); j += 1; k += 1 }
    output
  }

  def externalMergeSort(arr: Array[Int], M: Int, B: Int): Unit = {
    val n = arr.length
    if (n <= M) {
      quicksort(arr, 0, n - 1)
      return
    }

    // Step 1: Divide and sort runs
    val runs = (0 until n by M).map { i =>
      val size = math.min(M, n - i)
      val run = arr.slice(i, i + size)
      quicksort(run, 0, size - 1)
      run
    }.toList

    // Print sorted runs
    println("Sorted runs:")
    runs.zipWithIndex.foreach { case (run, i) => println(s"Run $i: ${run.mkString(" ")}") }

    // Step 2: Merge runs (2-way merge for M/B = 2)
    val output = mergeRuns(runs(0), runs(1), B)
    for (i <- 0 until n) arr(i) = output(i)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13)
    val M = 8
    val B = 4
    println(s"Initial array: ${arr.mkString(" ")}")
    externalMergeSort(arr, M, B)
    println(s"Sorted array: ${arr.mkString(" ")}")
  }
}
