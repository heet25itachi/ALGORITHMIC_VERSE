data class Bucket(var elements: MutableList<Int>, var pivot: Int)

fun approximateMedian(arr: List<Int>): Int {
    if (arr.isEmpty()) return 0
    val mid = arr.size / 2
    val values = listOf(arr[0], arr[mid], arr[arr.size - 1]).sorted()
    return values[1]
}

fun quicksort(arr: MutableList<Int>, left: Int, right: Int) {
    if (left >= right) return
    val pivot = arr[right]
    var i = left
    var j = right
    while (i < j) {
        while (i < j && arr[i] <= pivot) i++
        while (i < j && arr[j] > pivot) j--
        if (i < j) arr[i] = arr[j].also { arr[j] = arr[i] }
    }
    arr[i] = arr[right].also { arr[right] = arr[i] }
    quicksort(arr, left, i - 1)
    quicksort(arr, i + 1, right)
}

fun copyElems(arr: MutableList<Int>, next: MutableList<Int>, bnum: MutableList<Int>, buckets: MutableList<Bucket>, subarraySize: Int, bucketIdx: Int, sqrtN: Int) {
    while (next[bucketIdx] < subarraySize) {
        if (bnum[bucketIdx] >= buckets.size) {
            buckets.add(Bucket(mutableListOf(), 1_000_000_000))
        }
        if (arr[next[bucketIdx]] <= buckets[bnum[bucketIdx]].pivot) {
            if (buckets[bnum[bucketIdx]].elements.size >= 2 * sqrtN) {
                val median = approximateMedian(buckets[bnum[bucketIdx]].elements)
                val newBucket = Bucket(mutableListOf(), buckets[bnum[bucketIdx]].pivot)
                buckets[bnum[bucketIdx]].pivot = median
                val newElements = mutableListOf<Int>()
                for (x in buckets[bnum[bucketIdx]].elements) {
                    if (x <= median) newElements.add(x)
                    else newBucket.elements.add(x)
                }
                buckets[bnum[bucketIdx]].elements = newElements
                buckets.add(newBucket)
                for (i in bnum.indices) {
                    if (bnum[i] > bnum[bucketIdx]) bnum[i]++
                }
            }
            buckets[bnum[bucketIdx]].elements.add(arr[next[bucketIdx]])
            next[bucketIdx]++
        } else {
            bnum[bucketIdx]++
        }
    }
}

fun distribute(arr: MutableList<Int>, next: MutableList<Int>, bnum: MutableList<Int>, i: Int, j: Int, m: Int, buckets: MutableList<Bucket>, sqrtN: Int) {
    if (m == 1) {
        copyElems(arr.subList(i * sqrtN, (i + 1) * sqrtN), next, bnum, buckets, sqrtN, i, sqrtN)
    } else {
        distribute(arr, next, bnum, i, j, m / 2, buckets, sqrtN)
        distribute(arr, next, bnum, i + m / 2, j, m / 2, buckets, sqrtN)
        distribute(arr, next, bnum, i, j + m / 2, m / 2, buckets, sqrtN)
        distribute(arr, next, bnum, i + m / 2, j + m / 2, m / 2, buckets, sqrtN)
    }
}

fun cacheObliviousSort(arr: MutableList<Int>) {
    val n = arr.size
    if (n <= 1) return
    val sqrtN = kotlin.math.sqrt(n.toDouble()).toInt()
    if (sqrtN * sqrtN != n) return

    // Step 1: Partition and sort subarrays
    for (i in 0 until sqrtN) {
        quicksort(arr, i * sqrtN, i * sqrtN + sqrtN - 1)
    }

    // Step 2: Distribute
    val next = MutableList(sqrtN) { 0 }
    val bnum = MutableList(sqrtN) { 0 }
    val buckets = mutableListOf(Bucket(mutableListOf(), 1_000_000_000))
    distribute(arr, next, bnum, 0, 0, sqrtN, buckets, sqrtN)

    // Step 3: Sort buckets
    for (bucket in buckets) {
        bucket.elements.sort()
    }

    // Step 4: Concatenate
    var k = 0
    for (bucket in buckets) {
        for (x in bucket.elements) {
            arr[k++] = x
        }
    }
}

fun main() {
    val arr = mutableListOf(64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13)
    println("Initial array: $arr")
    cacheObliviousSort(arr)
    println("Sorted array: $arr")
}
