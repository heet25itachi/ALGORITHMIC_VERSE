package main

import (
    "fmt"
    "math/rand"
    "time"
)

const R = 65536

func birthdayAttack() {
    table := make(map[int]int64)
    rand.Seed(time.Now().UnixNano())
    trials := 0
    for {
        x := rand.Int63()
        h := x % R
        trials++
        if val, exists := table[h]; exists && val != x {
            fmt.Printf("Collision found after %d trials: x_i = %d, x_j = %d, hash = %d\n",
                trials, val, x, h)
            break
        }
        table[h] = x
    }
}

func main() {
    birthdayAttack()
}
