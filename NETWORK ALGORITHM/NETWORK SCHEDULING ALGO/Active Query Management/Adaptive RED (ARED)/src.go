package main

import (
    "fmt"
    "math/rand"
    "time"
)

type Packet struct {
    size int
    arrivalTime float64
}

type Queue struct {
    items []Packet
    capacity int
}

func NewQueue(capacity int) *Queue {
    return &Queue{items: make([]Packet, 0, capacity), capacity: capacity}
}

func (q *Queue) enqueue(p Packet) bool {
    if len(q.items) >= q.capacity {
        return false
    }
    q.items = append(q.items, p)
    return true
}

func (q *Queue) size() int {
    return len(q.items)
}

func (q *Queue) printQueue() {
    fmt.Print("Final queue: ")
    for _, p := range q.items {
        fmt.Printf("%d ", p.size)
    }
    fmt.Println()
}

func simulateARED(packets []Packet, minTh, maxTh, wq, target, alpha, beta, interval float64, capacity int) {
    q := NewQueue(capacity)
    avg, maxP, lastUpdate, currentTime := 0.0, 0.1, 0.0, 0.0
    count, dropped := 0, 0
    fmt.Println("Initial queue: empty")

    for _, p := range packets {
        avg = len(q.items) == 0 ? 0 : (1-wq)*avg + wq*float64(len(q.items))
        if currentTime-lastUpdate >= interval {
            if avg > target && maxP <= 0.5 {
                maxP *= (1 + alpha)
            } else if avg < target && maxP >= 0.01 {
                maxP *= beta
            }
            lastUpdate = currentTime
        }

        drop := false
        if avg < minTh {
            drop = false
        } else if avg >= maxTh {
            drop = true
        } else {
            pb := maxP * (avg - minTh) / (maxTh - minTh)
            pa := pb / (1 - float64(count)*pb)
            drop = rand.Float64() < pa
            count++
        }

        if drop {
            fmt.Printf("Packet dropped, size: %d, avg queue length: %.2f, max_p: %.4f\n", p.size, avg, maxP)
            dropped++
        } else if q.enqueue(p) {
            fmt.Printf("Packet enqueued, size: %d, avg queue length: %.2f, max_p: %.4f\n", p.size, avg, maxP)
            count = 0
        } else {
            fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
            dropped++
        }
        currentTime += 1.0
    }

    fmt.Printf("Final queue length: %d\n", q.size())
    fmt.Printf("Packets dropped: %d\n", dropped)
    q.printQueue()
}

func simulateBlue(packets []Packet, d1, d2, freezeTime float64, capacity int) {
    q := NewQueue(capacity)
    p, lastUpdate, currentTime := 0.0, 0.0, 0.0
    dropped := 0
    fmt.Println("Initial queue: empty")

    for _, p := range packets {
        if len(q.items) >= capacity {
            p += d1
            lastUpdate = currentTime
            fmt.Printf("Queue full, packet dropped, size: %d, drop prob: %.4f\n", p.size, p)
            dropped++
        } else {
            if currentTime-lastUpdate >= freezeTime && len(q.items) == 0 {
                if p > d2 {
                    p -= d2
                } else {
                    p = 0
                }
                lastUpdate = currentTime
            }
            if rand.Float64() < p {
                fmt.Printf("Packet dropped, size: %d, drop prob: %.4f\n", p.size, p)
                dropped++
            } else if q.enqueue(p) {
                fmt.Printf("Packet enqueued, size: %d, drop prob: %.4f\n", p.size, p)
            }
        }
        currentTime += 1.0
    }

    fmt.Printf("Final queue length: %d\n", q.size())
    fmt.Printf("Packets dropped: %d\n", dropped)
    q.printQueue()
}

func simulatePI(packets []Packet, qRef, a, b float64, capacity int) {
    q := NewQueue(capacity)
    p, prevError, currentTime := 0.0, 0.0, 0.0
    dropped := 0
    fmt.Println("Initial queue: empty")

    for _, p := range packets {
        error := float64(len(q.items)) - qRef
        p += a*error - b*prevError
        prevError = error
        if p < 0 {
            p = 0
        } else if p > 1 {
            p = 1
        }

        if rand.Float64() < p {
            fmt.Printf("Packet dropped, size: %d, drop prob: %.4f\n", p.size, p)
            dropped++
        } else if q.enqueue(p) {
            fmt.Printf("Packet enqueued, size: %d, drop prob: %.4f\n", p.size, p)
        } else {
            fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
            dropped++
        }
        currentTime += 1.0
    }

    fmt.Printf("Final queue length: %d\n", q.size())
    fmt.Printf("Packets dropped: %d\n", q.printQueue()
}

func main() {
    rand.Seed(time.Now().UnixNano())
    n, capacity := 200, 100
    packets := make([]Packet, n)
    for i := range packets {
        packets[i].size = rand.Intn(100) + 1
    }

    fmt.Println("=== ARED ===")
    simulateARED(packets, 20, 80, 0.002, 50, 0.01, 0.9, 1000, capacity)
    fmt.Println("\n=== Blue ===")
    simulateBlue(packets, 0.0002, 0.00005, 100, capacity)
    fmt.Println("\n=== PI ===")
    simulatePI(packets, 50, 0.00001822, 0.00001816, capacity)
}
