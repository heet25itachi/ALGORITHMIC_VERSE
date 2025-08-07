object Funnelsort {
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

  def kMerger(inputs: Array[Array[Int]], inputSizes: Array[Int], k: Int, buffer: Array[Int], bufferSize: Int, k3: Int): Array[Int] = {
    if (k == 1) {
      val size = math.min(inputSizes(0), k3)
      val output = inputs(0).take(size)
      inputs(0) = inputs(0).drop(size)
      inputSizes(0) -= size
      return output
    }

    val sqrtK = math.sqrt(k).toInt
    val inputMergers = Array.tabulate(sqrtK)(i => inputs(i * sqrtK))
    val inputMergerSizes = Array.tabulate(sqrtK)(i => inputSizes(i * sqrtK))
    val subBuffers = Array.fill(sqrtK, bufferSize)(0)
    val subBufferSizes = Array.fill(sqrtK)(0)

    val k32 = math.pow(k, 1.5).toInt
    for (i <- 0 until sqrtK) {
      if (subBufferSizes(i) < k32) {
        val tempOutput = kMerger(inputMergers, inputMergerSizes, sqrtK, subBuffers(i), bufferSize, k32)
        tempOutput.copyToArray(subBuffers(i))
        subBufferSizes(i) = tempOutput.length
      }
    }

    val outputMergerInputs = subBuffers
    val outputMergerSizes = subBufferSizes
    val output = kMerger(outputMergerInputs, outputMergerSizes, sqrtK, buffer, bufferSize, k3)

    for (i <- 0 until sqrtK) {
      subBuffers(i) = subBuffers(i).drop(output.length)
      subBufferSizes(i) -= output.length
    }

    output
  }

  def funnelsort(arr: Array[Int], z: Int, l: Int): Unit = {
    val n = arr.length
    if (n <= z) {
      quicksort(arr, 0, n - 1)
      return
    }

    val k = math.ceil(math.pow(n, 1.0 / 3.0)).toInt
    val subSize = math.ceil(n.toDouble / k).toInt
    val subarrays = Array.tabulate(k)(i => arr.slice(i * subSize, i * subSize + (if (i == k - 1) n - i * subSize else subSize)))
    val subarraySizes = Array.tabulate(k)(i => if (i == k - 1) n - i * subSize else subSize)

    println("Sorted subarrays:")
    for (i <- subarrays.indices) {
      quicksort(subarrays(i), 0, subarrays(i).length - 1)
      println(s"Subarray $i: ${subarrays(i).mkString(" ")}")
    }

    val bufferSize = 2 * math.pow(k, 1.5).toInt
    val buffer = Array.fill(bufferSize)(0)
    val output = kMerger(subarrays, subarraySizes, k, buffer, bufferSize, n)

    output.copyToArray(arr)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13)
    val z = 8
    val l = 2

    println("Initial array: " + arr.mkString(" "))

    funnelsort(arr, z, l)

    println("Sorted array: " + arr.mkString(" "))
  }
}
