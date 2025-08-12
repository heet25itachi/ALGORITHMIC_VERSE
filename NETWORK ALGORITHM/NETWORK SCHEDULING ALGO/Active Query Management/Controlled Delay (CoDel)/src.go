package main

import (
    "fmt"
    "math"
    "math/rand"
)

type Packet struct {
    size        int
    arrivalTime float64
}

type CoDelQueue struct {
    items          []Packet
    capacity       int
    firstAboveTime float64
    dropNext       float64
    dropCount      int
}

func NewCoDelQueue(capacity int) *CoDelQueue {
    return &CoDelQueue{items: []Packet{}, capacity: capacity, firstAboveTime: 0, dropNext: math.Inf(1), dropCount: 0}
}

func (q *CoDelQueue) enqueue(p Packet) bool {
    if len(q.items) >= q.capacity {
        fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
        return false
    }
    q.items = append(q.items, p)
    fmt.Printf("Packet enqueued, size: %d\n", p.size)
    return true
}

func (q *CoDelQueue) dequeue(currentTime float64, target float64, interval float64) {
    while len(q.items) > 0 {
        p := q.items[0]
        sojournTime := currentTime - p.arrivalTime

        if sojournTime < target || len(q.items) <= 4 {
            q.firstAboveTime = 0
            q.dropNext = math.Inf(1)
            q.items = q.items[1:]
            fmt.Printf("Packet dequeued, size: %d, sojourn time: %.2f\n", p.size, sojournTime)
            q.dropCount = 0
        } else if q.firstAboveTime == 0 {
            q.firstAboveTime = currentTime + interval
            q.dropNext = q.firstAboveTime
            q.items = q.items[1:]
            fmt.Printf("Packet dequeued, size: %d, sojourn time: %.2f\n", p.size, sojournTime)
        } else if currentTime >= q.dropNext {
            q.items = q.items[1:]
            fmt.Printf("Packet dropped, size: %d, sojourn time: %.2f\n", p.size, sojournTime)
            q.dropCount++
            q.dropNext = currentTime + interval / math.Sqrt(float64(q.dropCount))
        } else {
            q.items = q.items[1:]
            fmt.Printf("Packet dequeued, size: %d, sojourn time: %.2f\n", p.size, sojournTime)
            q.dropCount = 0
        }
    }
}

func (q *CoDelQueue) size() int {
    return len(q.items)
}

func simulateCoDel(packets []Packet, target float64, interval float64, capacity int) {
    q := NewCoDelQueue(capacity)
    currentTime := 0.0
    dropped := 0
    fmt.Println("Initial queue: empty")

    for _, p := range packets {
        packet := p
        packet.arrivalTime = currentTime
        if q.enqueue(packet) {
            q.dequeue(currentTime, target, interval)
        } else {
            dropped++
        }
        currentTime += 1.0
    }

    q.dequeue(currentTime, target, interval)

    fmt.Println("Final queue length: ", q.size())
    fmt.Println("Packets dropped: ", dropped)
    fmt.Println("Final queue: empty")
}

func main() {
    rand.Seed(42)
    packets := make([]Packet, 200)
    for i := range packets {
        packets[i].size = rand.Intn(100) + 1
    }
    simulateCoDel(packets, 5, 100, 100)
}
