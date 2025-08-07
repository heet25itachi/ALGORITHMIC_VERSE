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

fun mergeRuns(run1: List<Int>, run2: List<Int>, B: Int): List<Int> {
    val output = mutableListOf<Int>()
    var i = 0
    var j = 0
    while (i < run1.size && j < run2.size) {
        for (b in 0 until B) {
            if (i < run1.size && (j >= run2.size || run1[i] <= run2[j])) {
                output.add(run1[i++])
            } else if (j < run2.size) {
                output.add(run2[j++])
            }
        }
    }
    output.addAll(run1.subList(i, run1.size))
    output.addAll(run2.subList(j, run2.size))
    return output
}

fun externalMergeSort(arr: MutableList<Int>, M: Int, B: Int) {
    val n = arr.size
    if (n <= M) {
        quicksort(arr, 0, n - 1)
        return
    }

    // Step 1: Divide and sort runs
    val runs = mutableListOf<MutableList<Int>>()
    for (i in 0 until n step M) {
        val size = minOf(M, n - i)
        val run = arr.subList(i, i + size).toMutableList()
        quicksort(run, 0, size - 1)
        runs.add(run)
    }

    // Print sorted runs
    println("Sorted runs:")
    runs.forEachIndexed { i, run -> println("Run $i: $run") }

    // Step 2: Merge runs (2-way merge for M/B = 2)
    val output = mergeRuns(runs[0], runs[1], B)
    for (i in 0 until n) arr[i] = output[i]
}

fun main() {
    val arr = mutableListOf(64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13)
    val M = 8
    val B = 4
    println("Initial array: $arr")
    externalMergeSort(arr, M, B)
    println("Sorted array: $arr")
}
