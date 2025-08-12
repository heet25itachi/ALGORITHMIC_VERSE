package main

import (
    "fmt"
    "math/rand"
)

type Packet struct {
    size   int
    flowId int
}

type Bin struct {
    p          float64
    lastUpdate float64
}

type SFB {
    bins []Bin
    L    int
    N    int
}

func NewSFB(l, n int) *SFB {
    return &SFB{
        bins: make([]Bin, l*n),
        L: l,
        N: n,
    }
}

func (s *SFB) Drop(flowId int, currentTime, d1, d2, freezeTime float64, queueLength, capacity int) bool {
    marked := true
    for l := 0; l < s.L; l++ {
        binIdx := l * s.N + (flowId + l) % s.N // Simple hash
        bin := &s.bins[binIdx]
        if currentTime - bin.lastUpdate >= freezeTime {
            if queueLength == 0 {
                bin.p = max(bin.p - d2, 0.0)
            } else if queueLength >= capacity {
                bin.p += d1
            }
            bin.lastUpdate = currentTime
        }
        if bin.p < 1.0 {
            marked = false
            break
        }
    }
    return marked
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

func (q *Queue) Size() int {
    return len(q.items)
}

func simulateSFB(packets []Packet, d1, d2, freezeTime float64, capacity int, l, n int) {
    q := NewQueue(capacity)
    sfb := NewSFB(l, n)
    currentTime := 0.0
    dropped := 0
    fmt.Println("Initial queue: empty")

    rand.Seed(42)
    for _, p := range packets {
        if sfb.Drop(p.flowId, currentTime, d1, d2, freezeTime, q.Size(), capacity) {
            fmt.Printf("Packet dropped, size: %d, flow_id: %d\n", p.size, p.flowId)
            dropped++
        } else if q.Enqueue(p) {
            fmt.Printf("Packet enqueued, size: %d, flow_id: %d\n", p.size, p.flowId)
            // Immediate dequeue for simulation
            deqP := q.Dequeue()
            fmt.Printf("Packet dequeued, size: %d, flow_id: %d\n", deqP.size, deqP.flowId)
        } else {
            fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
            dropped++
        }
        currentTime += 1.0
    }

    while q.Size() > 0 {
        p := q.Dequeue()
        fmt.Printf("Packet dequeued, size: %d, flow_id: %d\n", p.size, p.flowId)
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
        packets[i].flowId = rand.Intn(20) + 1
    }

    fmt.Println("=== SFB ===")
    simulateSFB(packets, 0.0002, 0.00005, 100, 100, 2, 4)
}
