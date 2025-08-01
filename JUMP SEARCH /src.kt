import kotlin.math.sqrt
import kotlin.math.min

fun jumpSearch(arr: IntArray, target: Int): Int {
    val size = arr.size
    val step = sqrt(size.toDouble()).toInt()
    var prev = 0
    while (arr[min(step, size) - 1] < target) {
        prev = step
        step += sqrt(size.toDouble()).toInt()
        if (prev >= size) return -1
    }
    while (prev < size && arr[prev] < target) {
        prev++
    }
    if (prev < size && arr[prev] == target) return prev
    return -1
}

fun main() {
    val arr = intArrayOf(1, 3, 4, 7, 9)
    val target = 9
    val result = jumpSearch(arr, target)
    println("Target $target found at index: $result")
}
