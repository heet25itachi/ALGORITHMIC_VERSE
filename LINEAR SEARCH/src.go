package main

import "fmt"

func linearSearch(arr []int, target int) int {
    for i, val := range arr {
        if val == target {
            return i
        }
    }
    return -1
}

func main() {
    arr := []int{3, 7, 1, 9, 4}
    target := 9
    result := linearSearch(arr, target)
    fmt.Printf("Target %d found at index: %d\n", target, result)
}
