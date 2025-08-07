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
    arr[right], arr[i] = arr[i], arr[right]
    quicksort(arr, left, i-1)
    quicksort(arr, i+1, right)
}

func pemSelect(arr []int, n, k int) int {
    if n <= 5 {
        temp := make([]int, n)
        copy(temp, arr[:n])
        sort.Ints(temp)
        return temp[k-1]
    }
    return arr[k-1] // Simplified: use sort for small arrays
}

func pemMultipartition(arr []int, n int, pivots []int, dSqrt, p int) ([][]int, []int) {
    counts := make([]int, p*dSqrt)
    for i := 0; i < p; i++ {
        start := i * (n / p)
        size := n - start
        if i < p-1 {
            size = n / p
        }
        for j := 0; j < size; j++ {
            elem := arr[start+j]
            bucket := 0
            for bucket < dSqrt-1 && elem > pivots[bucket] {
                bucket++
            }
            counts[i*dSqrt+bucket]++
        }
    }

    prefixSums := make([]int, dSqrt)
    for j := 0; j < dSqrt; j++ {
        for i := 0; i < p; i++ {
            prefixSums[j] += counts[i*dSqrt+j]
        }
    }

    buckets := make([][]int, dSqrt)
    for j := 0; j < dSqrt; j++ {
        buckets[j] = make([]int, prefixSums[j])
    }

    offsets := make([]int, dSqrt)
    for i := 0; i < p; i++ {
        start := i * (n / p)
        size := n - start
        if i < p-1 {
            size = n / p
        }
        for j := 0; j < size; j++ {
            elem := arr[start+j]
            bucket := 0
            for bucket < dSqrt-1 && elem > pivots[bucket] {
                bucket++
            }
            buckets[bucket][offsets[bucket]] = elem
            offsets[bucket]++
        }
    }

    return buckets, prefixSums
}

func pemDistSort(arr []int, p, m, b, d int) {
    n := len(arr)
    if n <= m {
        quicksort(arr, 0, n-1)
        return
    }

    dSqrt := int(math.Sqrt(float64(d)))
    segmentSize := n / p

    fmt.Println("Segments:")
    for i := 0; i < p; i++ {
        size := n - i*segmentSize
        if i < p-1 {
            size = segmentSize
        }
        fmt.Printf("Segment %d: ", i)
        for j := 0; j < size; j++ {
            fmt.Printf("%d ", arr[i*segmentSize+j])
        }
        fmt.Println()
    }

    pivots := make([]int, dSqrt-1)
    for j := 0; j < dSqrt-1; j++ {
        pivots[j] = pemSelect(arr, n, (j+1)*n/dSqrt)
    }
    fmt.Print("Pivots: ")
    for _, x := range pivots {
        fmt.Printf("%d ", x)
    }
    fmt.Println()

    buckets, bucketSizes := pemMultipartition(arr, n, pivots, dSqrt, p)

    fmt.Println("Buckets:")
    for j := 0; j < dSqrt; j++ {
        fmt.Printf("Bucket %d: ", j)
        for _, x := range buckets[j] {
            fmt.Printf("%d ", x)
        }
        fmt.Println()
    }

    output := make([]int, n)
    outputPos := 0
    for j := 0; j < dSqrt; j++ {
        processors := int(math.Ceil(float64(len(buckets[j])) / float64(n/p)))
        pemDistSort(buckets[j], processors, m, b, d)
        copy(output[outputPos:], buckets[j])
        outputPos += len(buckets[j])
    }

    copy(arr, output)
}

func main() {
    arr := []int{64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13}
    n, p, m, b, d := len(arr), 4, 8, 2, 4

    fmt.Print("Initial array: ")
    for _, x := range arr {
        fmt.Printf("%d ", x)
    }
    fmt.Println()

    pemDistSort(arr, p, m, b, d)

    fmt.Print("Sorted array: ")
    for _, x := range arr {
        fmt.Printf("%d ", x)
    }
    fmt.Println()
}
