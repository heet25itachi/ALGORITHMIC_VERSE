package main

import (
    "container/list"
    "fmt"
    "math/rand"
    "time"
)

type Packet struct {
    size, priority, flowId int
}

type Queue struct {
    items *list.List
    capacity, totalBandwidth int
}

func NewQueue(capacity, totalBandwidth int) *Queue {
    return &Queue{items: list.New(), capacity: capacity, totalBandwidth: totalBandwidth}
}

func (q *Queue) Enqueue(p Packet) bool {
    if q.items.Len() >= q.capacity {
        return false
    }
    q.items.PushBack(p)
    return true
}

func (q *Queue) Dequeue() Packet {
    elem := q.items.Front()
    p := elem.Value.(Packet)
    q.items.Remove(elem)
    return p
}

func (q *Queue) Size() int {
    return q.items.Len()
}

type PRIQ struct {
    queues []*Queue
}

func NewPRIQ(numQueues, capacity, bandwidth int) *PRIQ {
    priq := &PRIQ{queues: make([]*Queue, numQueues)}
    for i := 0; i < numQueues; i++ {
        priq.queues[i] = NewQueue(capacity / numQueues, bandwidth / numQueues)
    }
    return priq
}

func (priq *PRIQ) Enqueue(p Packet) {
    pri := p.priority % len(priq.queues)
    if priq.queues[pri].Enqueue(p) {
        fmt.Printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", p.size, pri, p.flowId)
    } else {
        fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
    }
}

func (priq *PRIQ) DequeueAll() {
    for pri := len(priq.queues) - 1; pri >= 0; pri-- {
        for priq.queues[pri].Size() > 0 {
            p := priq.queues[pri].Dequeue()
            fmt.Printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, pri, p.flowId)
        }
    }
}

type CoDelQueue struct {
    q *Queue
    targetDelay, interval, firstAboveTime, dropNext float64
    dropCount int
}

func NewCoDelQueue(capacity, bandwidth int, target, interval float64) *CoDelQueue {
    return &CoDelQueue{
        q: NewQueue(capacity, bandwidth),
        targetDelay: target,
        interval: interval,
        firstAboveTime: 0.0,
        dropNext: math.Inf(1),
        dropCount: 0,
    }
}

func (c *CoDelQueue) Enqueue(p Packet) bool {
    if c.q.Enqueue(p) {
        fmt.Printf("Packet enqueued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId)
        c.processQueue(0.0)
        return true
    } else {
        fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
        return false
    }
}

func (c *CoDelQueue) processQueue(currentTime float64) {
    for c.q.Size() > 0 {
        // Simplified sojourn time
        sojournTime := currentTime - float64(c.dropCount) // Mock
        if sojournTime < c.targetDelay || c.q.Size() <= 4 {
            p := c.q.Dequeue()
            fmt.Printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId)
            c.firstAboveTime = 0.0
            c.dropNext = math.Inf(1)
            c.dropCount = 0
        } else if c.firstAboveTime == 0.0 {
            c.firstAboveTime = currentTime + c.interval
            c.dropNext = c.firstAboveTime
            p := c.q.Dequeue()
            fmt.Printf("Packet dequeued, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId)
        } else if currentTime >= c.dropNext {
            p := c.q.Dequeue()
            fmt.Printf("Packet dropped, size: %d, priority: %d, flow_id: %d\n", p.size, p.priority, p.flowId)
            c.dropCount++
            c.dropNext = currentTime + c.interval / math.Sqrt(float64(c.dropCount))
        } else {
            p := c.q.Dequeue()
            fmt.Printf("Packet dequeued, size: %d, priority: %d
