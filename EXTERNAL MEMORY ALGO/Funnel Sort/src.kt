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

fun kMerger(inputs: MutableList<IntArray>, inputSizes: IntArray, k: Int, buffer: IntArray, bufferSize: Int, k3: Int): IntArray {
    if (k == 1) {
        val size = minOf(inputSizes[0], k3)
        val output = inputs[0].copyOfRange(0, size)
        inputs[0] = inputs[0].copyOfRange(size, inputs[0].size)
        inputSizes[0] -= size
        return output
    }

    val sqrtK = sqrt(k.toDouble()).toInt()
    val inputMergers = MutableList(sqrtK) { inputs[it * sqrtK] }
    val inputMergerSizes = IntArray(sqrtK) { inputSizes[it * sqrtK] }
    val subBuffers = Array(sqrtK) { IntArray(bufferSize) }
    val subBufferSizes = IntArray(sqrtK)

    val k32 = k.toDouble().pow(1.5).toInt()
    for (i in 0 until sqrtK) {
        if (subBufferSizes[i] < k32) {
            val tempOutput = kMerger(inputMergers, inputMergerSizes, sqrtK, subBuffers[i], bufferSize, k32)
            tempOutput.copyInto(subBuffers[i], 0, 0, tempOutput.size)
            subBufferSizes[i] = tempOutput.size
        }
    }

    val outputMergerInputs = subBuffers.toMutableList()
    val outputMergerSizes = subBufferSizes.copyOf()
    val output = kMerger(outputMergerInputs, outputMergerSizes, sqrtK, buffer, bufferSize, k3)

    for (i in 0 until sqrtK) {
        subBuffers[i] = subBuffers[i].copyOfRange(output.size, subBuffers[i].size)
        subBufferSizes[i] -= output.size
    }

    return output
}

fun funnelsort(arr: IntArray, z: Int, l: Int) {
    val n = arr.size
    if (n <= z) {
        quicksort(arr, 0, n - 1)
        return
    }

    val k = ceil(n.toDouble().pow(1.0 / 3.0)).toInt()
    val subSize = ceil(n.toDouble() / k).toInt()
    val subarrays = mutableListOf<IntArray>()
    val subarraySizes = IntArray(k)

    println("Sorted subarrays:")
    for (i in 0 until k) {
        val size = if (i == k - 1) n - i * subSize else subSize
        val subarray = arr.copyOfRange(i * subSize, i * subSize + size)
        subarraySizes[i] = size
        quicksort(subarray, 0, size - 1)
        subarrays.add(subarray)
        println("Subarray $i: ${subarray.joinToString(" ")}")
    }

    val bufferSize = 2 * k.toDouble().pow(1.5).toInt()
    val buffer = IntArray(bufferSize)
    val output = kMerger(subarrays, subarraySizes, k, buffer, bufferSize, n)

    output.copyInto(arr)
}

fun main() {
    val arr = intArrayOf(64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13)
    val z = 8
    val l = 2

    println("Initial array: ${arr.joinToString(" ")}")

    funnelsort(arr, z, l)

    println("Sorted array: ${arr.joinToString(" ")}")
}
