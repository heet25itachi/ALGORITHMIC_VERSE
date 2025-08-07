object PEMDistSort {
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
    arr(right) = arr(i); arr(i) = pivot
    quicksort(arr, left, i - 1)
    quicksort(arr, i + 1, right)
  }

  def pemSelect(arr: Array[Int], n: Int, k: Int): Int = {
    if (n <= 5) {
      val temp = arr.take(n).sorted
      return temp(k - 1)
    }
    arr(k - 1) // Simplified: use direct access for small arrays
  }

  def pemMultipartition(arr: Array[Int], n: Int, pivots: Array[Int], dSqrt: Int, p: Int): (Array[Array[Int]], Array[Int]) = {
    val counts = Array.fill(p * dSqrt)(0)
    for (i <- 0 until p) {
      val start = i * (n / p)
      val size = if (i == p - 1) n - start else n / p
      for (j <- 0 until size) {
        val elem = arr(start + j)
        var bucket = 0
        while (bucket < dSqrt - 1 && elem > pivots(bucket)) bucket += 1
        counts(i * dSqrt + bucket) += 1
      }
    }

    val prefixSums = Array.fill(dSqrt)(0)
    for (j <- 0 until dSqrt) {
      for (i <- 0 until p) prefixSums(j) += counts(i * dSqrt + j)
    }

    val buckets = Array.tabulate(dSqrt)(j => Array.fill(prefixSums(j))(0))
    val bucketSizes = prefixSums.clone()

    val offsets = Array.fill(dSqrt)(0)
    for (i <- 0 until p) {
      val start = i * (n / p)
      val size = if (i == p - 1) n - start else n / p
      for (j <- 0 until size) {
        val elem = arr(start + j)
        var bucket = 0
        while (bucket < dSqrt - 1 && elem > pivots(bucket)) bucket += 1
        buckets(bucket)(offsets(bucket)) = elem
        offsets(bucket) += 1
      }
    }

    (buckets, bucketSizes)
  }

  def pemDistSort(arr: Array[Int], p: Int, m: Int, b: Int, d: Int): Unit = {
    val n = arr.length
    if (n <= m) {
      quicksort(arr, 0, n - 1)
      return
    }

    val dSqrt = math.sqrt(d).toInt
    val segmentSize = n / p

    println("Segments:")
    for (i <- 0 until p) {
      val size = if (i == p - 1) n - i * segmentSize else segmentSize
      println(s"Segment $i: ${arr.slice(i * segmentSize, i * segmentSize + size).mkString(" ")}")
    }

    val pivots = Array.tabulate(dSqrt - 1)(j => pemSelect(arr, n, (j + 1) * n / dSqrt))
    println(s"Pivots: ${pivots.mkString(" ")}")

    val (buckets, bucketSizes) = pemMultipartition(arr, n, pivots, dSqrt, p)

    println("Buckets:")
    for (j <- 0 until dSqrt) {
      println(s"Bucket $j: ${buckets(j).mkString(" ")}")
    }

    val output = Array.fill(n)(0)
    var outputPos = 0
    for (j <- 0 until dSqrt) {
      val processors = math.ceil(bucketSizes(j).toDouble / (n / p)).toInt
      pemDistSort(buckets(j), processors, m, b, d)
      buckets(j).copyToArray(output, outputPos)
      outputPos += bucketSizes(j)
    }

    output.copyToArray(arr)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13)
    val p = 4
    val m = 8
    val b = 2
    val d = 4

    println(s"Initial array: ${arr.mkString(" ")}")

    pemDistSort(arr, p, m, b, d)

    println(s"Sorted array: ${arr.mkString(" ")}")
  }
}
