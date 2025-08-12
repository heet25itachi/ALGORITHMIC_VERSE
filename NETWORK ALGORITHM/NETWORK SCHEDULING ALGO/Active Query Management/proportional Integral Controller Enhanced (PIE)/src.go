package main

import (
    "fmt"
    "math/rand"
)

// Packet and Queue types
type Packet struct {
    size        int
    arrivalTime float64
}

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

func (q *Queue) Dequeue() Packet {
    p := q.items[0]
    q.items = q.items[1:]
    return p
}

func (q *Queue) Peek() Packet {
    return q.items[0]
}

func (q *Queue) Size() int {
    return len(q.items)
}

// Simulate PIE algorithm
func simulatePie(packets []Packet, target, updateInterval, alpha, beta, maxDropProb, maxBurst float64, capacity int) {
    q := NewQueue(capacity)
    currentTime, lastUpdate, dropProb, prevDelay, burstTime := 0.0, 0.0, 0.0, 0.0, maxBurst
    dropped := 0
    fmt.Println("Initial queue: empty")
    rand.Seed(42)

    for _, p := range packets {
        p.arrivalTime = currentTime
        delay := 0.0
        if q.Size() > 0 {
            delay = currentTime - q.Peek().arrivalTime
        }

        if currentTime-lastUpdate >= updateInterval {
            error := delay - target
            dropProb += alpha*error + beta*(delay-prevDelay)
            if dropProb < 0 {
                dropProb = 0
            }
            if dropProb > maxDropProb {
                dropProb = maxDropProb
            }
            prevDelay = delay
            lastUpdate = currentTime
            if delay > target {
                burstTime = 0
            } else if burstTime < maxBurst {
                burstTime += updateInterval
            }
        }

        drop := burstTime < maxBurst && delay > target && rand.Float64() < dropProb

        if drop {
            fmt.Printf("Packet dropped, size: %d, queue delay: %.2f, drop prob: %.4f\n", p.size, delay, dropProb)
            dropped++
        } else if q.Enqueue(p) {
            fmt.Printf("Packet enqueued, size: %d, queue delay: %.2f, drop prob: %.4f\n", p.size, delay, dropProb)
            deqP := q.Dequeue()
            fmt.Printf("Packet dequeued, size: %d, queue delay: %.2f\n", deqP.size, delay)
        } else {
            fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
            dropped++
        }
        currentTime += 1.0
    }

    for q.Size() > 0 {
        delay := currentTime - q.Peek().arrivalTime
        if currentTime-lastUpdate >= updateInterval {
            error := delay - target
            dropProb += alpha*error + beta*(delay-prevDelay)
            if dropProb < 0 {
                dropProb = 0
            }
            if dropProb > maxDropProb {
                dropProb = maxDropProb
            }
            prevDelay = delay
            lastUpdate = currentTime
            if delay > target {
                burstTime = 0
            } else if burstTime < maxBurst {
                burstTime += updateInterval
            }
        }
        deqP := q.Dequeue()
        fmt.Printf("Packet dequeued, size: %d, queue delay: %.2f\n", deqP.size, delay)
        currentTime += 1.0
    }

    fmt.Printf("Final queue length: %d\n", q.Size())
    fmt.Printf("Packets dropped: %d\n", dropped)
    fmt.Println("Final queue: empty")
}

func main() {
    packets := make([]Packet, 200)
    for i := range packets {
        packets[i].size = rand.Intn(100) + 1
    }
    fmt.Println("=== PIE ===")
    simulatePie(packets, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100)
}
