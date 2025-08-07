package main

import (
    "fmt"
    "math/rand"
    "time"
)

type Packet struct {
    size int
}

type REDQueue struct {
    items    []int
    capacity int
    avg      float64
    count    int
}

func NewREDQueue(capacity int) *REDQueue {
    return &REDQueue{items: make([]int, 0, capacity), capacity: capacity, avg: 0, count: 0}
}

func (q *REDQueue) enqueue(p Packet, minTh, maxTh, wq, maxP float64) bool {
    if len(q.items) == 0 {
        q.avg = 0
    } else {
        q.avg = (1-wq)*q.avg + wq*float64(len(q.items))
    }

    drop := false
    if q.avg < minTh {
        drop = false
    } else if q.avg >= maxTh {
        drop = true
    } else {
        pb := maxP * (q.avg - minTh) / (maxTh - minTh)
        pa := pb / (1 - float64(q.count)*pb)
        if rand.Float64() < pa {
            drop = true
        } else {
            drop = false
        }
        q.count++
    }

    if drop {
        fmt.Printf("Packet dropped, size: %d, avg queue length: %.2f\n", p.size, q.avg)
        return false
    } else if len(q.items) < q.capacity {
        q.items = append(q.items, p.size)
        fmt.Printf("Packet enqueued, size: %d, avg queue length: %.2f\n", p.size, q.avg)
        q.count = 0
        return true
    } else {
        fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
        return false
    }
}

func (q *REDQueue) size() int {
    return len(q
