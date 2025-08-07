package main

import (
    "fmt"
    "sort"
)

func removeDuplicates(arr []int) []int {
    sort.Ints(arr)
    result := []int{}
    for i, v := range arr {
        if i == 0 || v != arr[i-1] {
            result = append(result, v)
        }
    }
    return result
}

func setDifference(a, b []int) []int {
    result := []int{}
    bSet := make(map[int]bool)
    for _, v := range b {
        bSet[v] = true
    }
    for _, v := range a {
        if !bSet[v] {
            result = append(result, v)
        }
    }
    return result
}

func externalBFS(adj [][]int, start, M, B int) {
    n := len(adj)
    visited := make([]bool, n)
    level := make([]int, M)
    prevLevel := make([]int, M)
    prevPrevLevel := make([]int, M)
    levelSize, prevLevelSize, prevPrevLevelSize := 0, 0, 0
    t := 0

    // Initialize L(0) = {start}
    level[0] = start
    levelSize = 1
    visited[start] = true
    fmt.Printf("Level %d: %d\n", t, start)

    for levelSize > 0 {
        // Step 1: Compute A(t) = neighbors of L(t-1)
        neighbors := []int{}
        for i := 0; i < levelSize; i++ {
            v := level[i]
            neighbors = append(neighbors, adj[v]...)
        }

        // Step 2: Compute A'(t) by removing duplicates
        neighbors = removeDuplicates(neighbors)

        // Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
        temp := append(prevLevel[:prevLevelSize], prevPrevLevel[:prevPrevLevelSize]...)
        temp = removeDuplicates(temp)
        newLevel := setDifference(neighbors, temp)

        // Update visited
        for _, v := range newLevel {
            visited[v] = true
        }

        // Print current level
        if len(newLevel) > 0 {
            fmt.Printf("Level %d: ", t+1)
            for _, v := range newLevel {
                fmt.Printf("%d ", v)
            }
            fmt.Println()
        }

        // Update levels
        prevPrevLevelSize = prevLevelSize
        copy(prevPrevLevel, prevLevel)
        prevLevelSize = levelSize
        copy(prevLevel, level)
        levelSize = len(newLevel)
        copy(level, newLevel)
        t++
    }
}

func main() {
    n := 10
    adj := make([][]int, n)
    edges := [][2]int{{0,1}, {0,3}, {0,9}, {1,0}, {1,2}, {1,4}, {2,1}, {2,3},
                      {3,0}, {3,2}, {3,4}, {4,1}, {4,3}, {4,5}, {5,4}, {5,6},
                      {5,8}, {6,5}, {6,7}, {7,6}, {7,8}, {8,5}, {8,7}, {8,9},
                      {9,0}, {9,8}}
    for _, e := range edges {
        adj[e[0]] = append(adj[e[0]], e[1])
    }

    fmt.Println("Adjacency lists:")
    for i := 0; i < n; i++ {
        fmt.Printf("%d: %v\n", i, adj[i])
    }

    externalBFS(adj, 0, 5, 2)
}
