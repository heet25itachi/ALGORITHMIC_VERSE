case class Bucket(var elements: List[Int], var pivot: Int)

object CacheObliviousSort {
  def approximateMedian(arr: List[Int]): Int = {
    if (arr.isEmpty) 0
    else {
      val mid = arr.length / 2
      List(arr(0), arr(mid), arr(arr.length - 1)).sorted.apply(1)
    }
  }

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

  def copyElems(arr: Array[Int], next: Array[Int], bnum: Array[Int], buckets: scala.collection.mutable.ListBuffer[Bucket], subarraySize: Int, bucketIdx: Int, sqrtN: Int): Unit = {
    while (next(bucketIdx) < subarraySize) {
      if (bnum(bucketIdx) >= buckets.length) {
        buckets += Bucket(List(), 1000000000)
      }
      if (arr(next(bucketIdx)) <= buckets(bnum(bucketIdx)).pivot) {
        if (buckets(bnum(bucketIdx)).elements.length >= 2 * sqrtN) {
          val median = approximateMedian(buckets(bnum(bucketIdx)).elements)
          val newBucket = Bucket(List(), buckets(bnum(bucketIdx)).pivot)
          buckets(bnum(bucketIdx)).pivot = median
          val newElements = buckets(bnum(bucketIdx)).elements.filter(_ <= median)
          newBucket.elements = buckets(bnum(bucketIdx)).elements.filter(_ > median)
          buckets(bnum(bucketIdx)).elements = newElements
          buckets += newBucket
          bnum.indices.foreach(i => if (bnum(i) > bnum(bucketIdx)) bnum(i) += 1)
        }
        buckets(bnum(bucketIdx)).elements = arr(next(bucketIdx)) :: buckets(bnum(bucketIdx)).elements
        next(bucketIdx) += 1
      } else {
        bnum(bucketIdx) += 1
      }
    }
  }

  def distribute(arr: Array[Int], next: Array[Int], bnum: Array[Int], i: Int, j: Int, m: Int, buckets: scala.collection.mutable.ListBuffer[Bucket], sqrtN: Int): Unit = {
    if (m == 1) {
      copyElems(arr.slice(i * sqrtN, (i + 1) * sqrtN), next, bnum, buckets, sqrtN, i, sqrtN)
    } else {
      distribute(arr, next, bnum, i, j, m / 2, buckets, sqrtN)
      distribute(arr, next, bnum, i + m / 2, j, m / 2, buckets, sqrtN)
      distribute(arr, next, bnum, i, j + m / 2, m / 2, buckets, sqrtN)
      distribute(arr, next, bnum, i + m / 2, j + m / 2, m / 2, buckets, sqrtN)
    }
  }

  def cacheObliviousSort(arr: Array[Int]): Unit = {
    val n = arr.length
    if (n <= 1) return
    val sqrtN = math.sqrt(n.toDouble).toInt
    if (sqrtN * sqrtN != n) return

    // Step 1: Partition and sort subarrays
    for (i <- 0 until sqrtN) {
      quicksort(arr, i * sqrtN, i * sqrtN + sqrtN - 1)
    }

    // Step 2: Distribute
    val next = Array.fill(sqrtN)(0)
    val bnum = Array.fill(sqrtN)(0)
    val buckets = scala.collection.mutable.ListBuffer(Bucket(List(), 1000000000))
    distribute(arr, next, bnum, 0, 0, sqrtN, buckets, sqrtN)

    // Step 3: Sort buckets
    buckets.foreach(bucket => bucket.elements = bucket.elements.sorted)

    // Step 4: Concatenate
    var k = 0
    for (bucket <- buckets; x <- bucket.elements) {
      arr(k) = x
      k += 1
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13)
    println(s"Initial array: ${arr.mkString(" ")}")
    cacheObliviousSort(arr)
    println(s"Sorted array: ${arr.mkString(" ")}")
  }
}
