import kotlin.math.*

fun quicksort(arr: IntArray, left: Int, right: Int) {
    if (left >= right) return
    val pivot = arr[right]
    var i = left
    var j = right
    while (i < j) {
        while (i < j && arr[i] <= pivot) i++
        while (i < j && arr[j] > pivot) j--
        if (i < j) arr[i] = arr[j].also { arr[j] = arr[i] }
    }
    arr[right] = arr[i]
    arr[i] = pivot
    quicksort(arr, left, i - 1)
    quicksort(arr, i + 1, right)
}

fun pemSelect(arr: IntArray, n: Int, k: Int): Int {
    if (n <= 5) {
        val temp = arr.copyOf(n).sorted()
        return temp[k - 1]
    }
    return arr[k - 1] // Simplified: use direct access for small arrays
}

fun pemMultipartition(arr: IntArray, n: Int, pivots: IntArray, dSqrt: Int, p: Int): Pair<List<IntArray>, IntArray> {
    val counts = IntArray(p * dSqrt)
    for (i in 0 until p) {
        val start = i * (n / p)
        val size = if (i == p - 1) n - start else n / p
        for (j in 0 until size) {
            val elem = arr[start + j]
            var bucket = 0
            while (bucket < dSqrt - 1 && elem > pivots[bucket]) bucket++
            counts[i * dSqrt + bucket]++
        }
    }

    val prefixSums = IntArray(dSqrt)
    for (j in 0 until dSqrt) {
        for (i in 0 until p) prefixSums[j] += counts[i * dSqrt + j]
    }

    val buckets = List(dSqrt) { IntArray(prefixSums[it]) }
    val bucketSizes = prefixSums.copyOf()

    val offsets = IntArray(dSqrt)
    for (i in 0 until p) {
        val start = i * (n / p)
        val size = if (i == p - 1) n - start else n / p
        for (j in 0 until size) {
            val elem = arr[start + j]
            var bucket = 0
            while (bucket < dSqrt - 1 && elem > pivots[bucket]) bucket++
            buckets[bucket][offsets[bucket]++] = elem
        }
    }

    return Pair(buckets, bucketSizes)
}

fun pemDistSort(arr: IntArray, p: Int, m: Int, b: Int, d: Int) {
    val n = arr.size
    if (n <= m) {
        quicksort(arr, 0, n - 1)
        return
    }

    val dSqrt = sqrt(d.toDouble()).toInt()
    val segmentSize = n / p

    println("Segments:")
    for (i in 0 until p) {
        val size = if (i == p - 1) n - i * segmentSize else segmentSize
        print("Segment $i: ")
        for (j in 0 until size) print("${arr[i * segmentSize + j]} ")
        println()
    }

    val pivots = IntArray(dSqrt - 1) { pemSelect(arr, n, (it + 1) * n / dSqrt) }
    print("Pivots: ${pivots.joinToString(" ")}\n")

    val (buckets, bucketSizes) = pemMultipartition(arr, n, pivots, dSqrt, p)

    println("Buckets:")
    for (j in 0 until dSqrt) {
        print("Bucket $j: ${buckets[j].joinToString(" ")}\n")
    }

    val output = IntArray(n)
    var outputPos = 0
    for (j in 0 until dSqrt) {
        val processors = ceil(bucketSizes[j].toDouble() / (n / p)).toInt()
        pemDistSort(buckets[j], processors, m, b, d)
        buckets[j].copyInto(output, outputPos, 0, bucketSizes[j])
        outputPos += bucketSizes[j]
    }

    output.copyInto(arr)
}

fun main() {
    val arr = intArrayOf(64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13)
    val p = 4
    val m = 8
    val b = 2
    val d = 4

    println("Initial array: ${arr.joinToString(" ")}")

    pemDistSort(arr, p, m, b, d)

    println("Sorted array: ${arr.joinToString(" ")}")
}
