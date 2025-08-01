package main

import (
    "fmt"
    "math"
)

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}

func jumpSearch(arr []int, target int) int {
    size := len(arr)
    step := int(math.Sqrt(float64(size)))
    prev := 0
    for arr[min(step, size)-1] < target {
        prev = step
        step += int(math.Sqrt(float64(size)))
        if prev >= size {
            return -1
        }
    }
    for prev < size && arr[prev] < target {
        prev++
    }
    if prev < size && arr[prev] == target {
        return prev
    }
    return -1
}

func main() {
    arr := []int{1, 3, 4, 7, 9}
    target := 9
    result := jumpSearch(arr, target)
    fmt.Printf("Target %d found at index: %d\n", target, result)
}
