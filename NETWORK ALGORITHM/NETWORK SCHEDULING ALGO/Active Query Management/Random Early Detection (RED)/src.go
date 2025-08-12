package main

import (
    "fmt"
    "math/rand"
)

// Packet and Queue types
type Packet struct{ size int }
type Queue struct {
    items    []Packet
    capacity int
}

func NewQueue(capacity int) *Queue {
    return &Queue{items: []Packet{}, capacity: capacity}
}

func (q *Queue) Enqueue(p Packet) bool {
    if len(q.items) >= q.capacity {
        return false
    }
    q.items = append(q.items, p)
    return true
}

func (q *Queue) Size() int {
    return len(q.items)
}

func (q *Queue) PrintQueue() {
    fmt.Print("Final queue: ")
    for _, p := range q.items {
        fmt.Print(p.size, " ")
    }
    fmt.Println()
}

// Simulate RED algorithm
func simulateRed(packets []Packet, minTh, maxTh, wq, maxP float64, capacity int) {
    q := NewQueue(capacity)
    avg, count, dropped := 0.0, 0.0, 0.0
    fmt.Println("Initial queue: empty")
    for _, p := range packets {
        if q.Size() == 0 {
            avg = 0
        } else {
            avg = (1-wq)*avg + wq*float64(q.Size())
        }
        drop := false
        if avg < minTh {
            drop = false
        } else if avg >= maxTh {
            drop = true
        } else {
            pb := maxP * (avg - minTh) / (maxTh - minTh)
            pa := pb / (1 - count*pb)
            count++
            drop = rand.Float64() < pa
        }
        if drop {
            fmt.Printf("Packet dropped, size: %d, avg queue length: %.2f, max_p: %.4f\n", p.size, avg, maxP)
            dropped++
        } else if q.Enqueue(p) {
            fmt.Printf("Packet enqueued, size: %d, avg queue length: %.2f, max_p: %.4f\n", p.size, avg, maxP)
            count = 0
        } else {
            fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
            dropped++
        }
    }
    fmt.Printf("Final queue length: %d\n", q.Size())
    fmt.Printf("Packets dropped: %d\n", int(dropped))
    q.PrintQueue()
}

func main() {
    rand.Seed(42)
    packets := make([]Packet, 200)
    for i := range packets {
        packets[i] = Packet{rand.Intn(100) + 1}
    }
    fmt.Println("=== RED ===")
    simulateRed(packets, 20, 80, 0.002, 0.1, 100)
}
