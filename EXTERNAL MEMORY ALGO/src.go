package main

import (
    "fmt"
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
    arr[i], arr[right] = arr[right], arr[i]
    quicksort(arr, left, i-1)
    quicksort(arr, i+1, right)
}

func mergeRuns(run1, run2 []int, B int) []int {
    output := make([]int, 0, len(run1)+len(run2))
    i, j := 0, 0
    for i < len(run1) && j < len(run2) {
        for b := 0; b < B && i < len(run1) && j < len(run2); b++ {
            if run1[i] <= run2[j] {
                output = append(output, run1[i])
                i++
            } else {
                output = append(output, run2[j])
                j++
            }
        }
    }
    output = append(output, run1[i:]...)
    output = append(output, run2[j:]...)
    return output
}

func externalMergeSort(arr []int, M, B int) {
    n := len(arr)
    if n <= M {
        quicksort(arr, 0, n-1)
        return
    }

    // Step 1: Divide and sort runs
    runs := [][]int{}
    for i := 0; i < n; i += M {
        size := M
        if i+M > n {
            size = n - i
        }
        run := make([]int, size)
        copy(run, arr[i:i+size])
        quicksort(run, 0, size-1)
        runs = append(runs, run)
    }

    // Print sorted runs
    fmt.Println("Sorted runs:")
    for i, run := range runs {
        fmt.Printf("Run %d: %v\n", i, run)
    }

    // Step 2: Merge runs (2-way merge for M/B = 2)
    output := mergeRuns(runs[0], runs[1], B)
    copy(arr, output)
}

func main() {
    arr := []int{64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13}
    M, B := 8, 4
    fmt.Println("Initial array:", arr)
    externalMergeSort(arr, M, B)
    fmt.Println("Sorted array:", arr)
}
