package main

import (
    "fmt"
    "math"
)

func quicksort(arr []int, left, right int) {
    if left >= right {
        return
    }
    pivot := arr[right]
    i, j := left, right
    for i < j {
        for i < j && arr[i] <= pivot {
            i++
        }
        for i < j && arr[j] > pivot {
            j--
        }
        if i < j {
            arr[i], arr[j] = arr[j], arr[i]
        }
    }
    arr[right], arr[i] = arr[i], pivot
    quicksort(arr, left, i-1)
    quicksort(arr, i+1, right)
}

func kMerger(inputs [][]int, inputSizes []int, k int, buffer []int, bufferSize int, k3 int) []int {
    if k == 1 {
        size := inputSizes[0]
        if size > k3 {
            size = k3
        }
        output := make([]int, size)
        copy(output, inputs[0][:size])
        inputs[0] = inputs[0][size:]
        inputSizes[0] -= size
        return output
    }

    sqrtK := int(math.Sqrt(float64(k)))
    inputMergers := make([][]int, sqrtK)
    inputMergerSizes := make([]int, sqrtK)
    subBuffers := make([][]int, sqrtK)
    subBufferSizes := make([]int, sqrtK)
    for i := 0; i < sqrtK; i++ {
        inputMergers[i] = inputs[i*sqrtK]
        inputMergerSizes[i] = inputSizes[i*sqrtK]
        subBuffers[i] = make([]int, bufferSize)
    }

    k32 := int(math.Pow(float64(k), 1.5))
    for i := 0; i < sqrtK; i++ {
        if subBufferSizes[i] < k32 {
            tempOutput := kMerger(inputMergers, inputMergerSizes, sqrtK, subBuffers[i], bufferSize, k32)
            copy(subBuffers[i], tempOutput)
            subBufferSizes[i] = len(tempOutput)
        }
    }

    outputMergerInputs := subBuffers
    outputMergerSizes := subBufferSizes
    output := kMerger(outputMergerInputs, outputMergerSizes, sqrtK, buffer, bufferSize, k3)

    for i := 0; i < sqrtK; i++ {
        subBuffers[i] = subBuffers[i][len(output):]
        subBufferSizes[i] -= len(output)
    }

    return output
}

func funnelsort(arr []int, z, l int) {
    n := len(arr)
    if n <= z {
        quicksort(arr, 0, n-1)
        return
    }

    k := int(math.Ceil(math.Pow(float64(n), 1.0/3.0)))
    subSize := int(math.Ceil(float64(n) / float64(k)))
    subarrays := make([][]int, k)
    subarraySizes := make([]int, k)

    fmt.Println("Sorted subarrays:")
    for i := 0; i < k; i++ {
        size := subSize
        if i == k-1 {
            size = n - i*subSize
        }
        subarrays[i] = arr[i*subSize : i*subSize+size]
        subarraySizes[i] = size
        quicksort(subarrays[i], 0, size-1)
        fmt.Printf("Subarray %d: ", i)
        for _, x := range subarrays[i] {
            fmt.Printf("%d ", x)
        }
        fmt.Println()
    }

    bufferSize := 2 * int(math.Pow(float64(k), 1.5))
    buffer := make([]int, bufferSize)
    output := kMerger(subarrays, subarraySizes, k, buffer, bufferSize, n)

    copy(arr, output)
}

func main() {
    arr := []int{64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13}
    n, z, l := len(arr), 8, 2

    fmt.Print("Initial array: ")
    for _, x := range arr {
        fmt.Printf("%d ", x)
    }
    fmt.Println()

    funnelsort(arr, z, l)

    fmt.Print("Sorted array: ")
    for _, x := range arr {
        fmt.Printf("%d ", x)
    }
    fmt.Println()
}
