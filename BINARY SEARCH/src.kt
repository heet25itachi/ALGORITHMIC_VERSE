fun binarySearch(arr: IntArray, target: Int): Int {
    var left = 0
    var right = arr.size - 1
    while (left <= right) {
        val mid = (left + right) / 2
        if (arr[mid] == target) {
            return mid
        } else if (arr[mid] < target) {
            left = mid + 1
        } else {
            right = mid - 1
        }
    }
    return -1
}

fun main() {
    val arr = intArrayOf(1, 3, 4, 7, 9)
    val target = 9
    val result = binarySearch(arr, target)
    println("Target $target found at index: $result")
}
