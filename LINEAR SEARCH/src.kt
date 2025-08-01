fun linearSearch(arr: IntArray, target: Int): Int {
    for (i in arr.indices) {
        if (arr[i] == target) {
            return i
        }
    }
    return -1
}

fun main() {
    val arr = intArrayOf(3, 7, 1, 9, 4)
    val target = 9
    val result = linearSearch(arr, target)
    println("Target $target found at index: $result")
}
