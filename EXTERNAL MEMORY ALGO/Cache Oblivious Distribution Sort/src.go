package main

import (
    "fmt"
    "math"
)

type Bucket struct {
    elements []int
    pivot    int
}

func approximateMedian(arr []int) int {
    if len(arr) == 0 {
        return 0
    }
    mid := len(arr) / 2
    values := []int{arr[0], arr[mid], arr[len(arr)-1]}
    sort.Ints(values)
    return values[1]
}

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

func copyElems(arr []int, next, bnum *int, buckets *[]Bucket, subarraySize, bucketIdx, sqrtN int) {
    for *next < subarraySize {
        if *bnum >= len(*buckets) {
            *buckets = append(*buckets, Bucket{make([]int, 0), int(1e9)})
        }
        if arr[*next] <= (*buckets)[*bnum].pivot {
            if len((*buckets)[*bnum].elements) >= 2*sqrtN {
                median := approximateMedian((*buckets)[*bnum].elements)
                newBucket := Bucket{make([]int, 0), (*buckets)[*bnum].pivot}
                (*buckets)[*bnum].pivot = median
                newElements := []int{}
                for _, x := range (*buckets)[*bnum].elements {
                    if x <= median {
                        newElements = append(newElements, x)
                    } else {
                        newBucket.elements = append(newBucket.elements, x)
                    }
                }
                (*buckets)[*bnum].elements = newElements
                *buckets = append(*buckets, newBucket)
                for i := range bnum {
                    if bnum[i] > *bnum {
                        bnum[i]++
                    }
                }
            }
            (*buckets)[*bnum].elements = append((*buckets)[*bnum].elements, arr[*next])
            *next++
        } else {
            *bnum++
        }
    }
}

func distribute(arr []int, next, bnum []int, i, j, m int, buckets *[]Bucket, sqrtN int) {
    if m == 1 {
        copyElems(arr[i*sqrtN:], &next[i], &bnum[i], buckets, sqrtN, i, sqrtN)
    } else {
        distribute(arr, next, bnum, i, j, m/2, buckets, sqrtN)
        distribute(arr, next, bnum, i+m/2, j, m/2, buckets, sqrtN)
        distribute(arr, next, bnum, i, j+m/2, m/2, buckets, sqrtN)
        distribute(arr, next, bnum, i+m/2, j+m/2, m/2, buckets, sqrtN)
    }
}

func cacheObliviousSort(arr []int) {
    n := len(arr)
    if n <= 1 {
        return
    }
    sqrtN := int(math.Sqrt(float64(n)))
    if sqrtN*sqrtN != n {
        return
    }

    // Step 1: Partition and sort subarrays
    for i := 0; i < sqrtN; i++ {
        quicksort(arr, i*sqrtN, i*sqrtN+sqrtN-1)
    }

    // Step 2: Distribute
    next := make([]int, sqrtN)
    bnum := make([]int, sqrtN)
    buckets := []Bucket{{make([]int, 0), int(1e9)}}
    distribute(arr, next, bnum, 0, 0, sqrtN, &buckets, sqrtN)

    // Step 3: Sort buckets
    for i := range buckets {
        quicksort(buckets[i].elements, 0, len(buckets[i].elements)-1)
    }

    // Step 4: Concatenate
    k := 0
    for _, bucket := range buckets {
        for _, x := range bucket.elements {
            arr[k] = x
            k++
        }
    }
}

func main() {
    arr := []int{64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13}
    fmt.Println("Initial array:", arr)
    cacheObliviousSort(arr)
    fmt.Println("Sorted array:", arr)
}
