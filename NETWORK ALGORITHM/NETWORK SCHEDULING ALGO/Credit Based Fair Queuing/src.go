package main

import (
	"fmt"
	"math"
	"math/rand"
	"time"
)

type Packet struct {
	size, weight, flowId int
}

type Queue struct {
	items          []Packet
	capacity       int
	credit         float64
	creditRate     float64
	totalBandwidth int
}

func NewQueue(capacity int, creditRate float64, totalBandwidth int) *Queue {
	return &Queue{
		items:          make([]Packet, 0, capacity),
		capacity:       capacity,
		credit:         0.0,
		creditRate:     creditRate,
		totalBandwidth: totalBandwidth,
	}
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

func (q *Queue) UpdateCredit(deltaTime float64) {
	q.credit += q.creditRate * deltaTime
	if q.credit < 0 {
		q.credit = 0.0
	}
}

func (q *Queue) Size() int {
	return len(q.items)
}

func simulateCBFQ(packets []Packet, capacity, bandwidth int, baseRate float64) {
	weights := []float64{1.0, 2.0, 3.0, 4.0, 5.0}
	queues := make([]*Queue, 5)
	for i := 0; i < 5; i++ {
		queues[i] = NewQueue(capacity/5, weights[i]*baseRate, bandwidth/5)
	}
	currentTime := 0.0
	serviceRate := 1000.0
	dropped := 0
	fmt.Println("=== CBFQ Scheduler ===")
	fmt.Println("Initial queue: empty")

	for _, p := range packets {
		queueIdx := p.flowId % 5
		queues[queueIdx].UpdateCredit(0.001)

		if queues[queueIdx].Enqueue(p) {
			fmt.Printf("Packet enqueued, size: %d, weight: %.1f, flow_id: %d, credit: %.2f\n",
				p.size, weights[queueIdx], p.flowId, queues[queueIdx].credit)
		} else {
			fmt.Printf("Queue full, packet dropped, size: %d\n", p.size)
			dropped++
		}

		maxCredit := -1.0
		maxIdx := -1
		for j := 0; j < 5; j++ {
			if queues[j].Size() > 0 && queues[j].credit > maxCredit {
				maxCredit = queues[j].credit
				maxIdx = j
			}
		}
		if maxIdx != -1 {
			p := queues[maxIdx].Dequeue()
			queues[maxIdx].credit -= serviceRate * 0.001
			if queues[maxIdx].credit < 0 {
				queues[maxIdx].credit = 0.0
			}
			fmt.Printf("Packet dequeued, size: %d, weight: %.1f, flow_id: %d, credit: %.2f\n",
				p.size, weights[maxIdx], p.flowId, queues[maxIdx].credit)
		}
		currentTime += 0.001
	}

	for queueIdx := 0; queueIdx < 5; queueIdx++ {
		for queues[queueIdx].Size() > 0 {
			p := queues[queueIdx].Dequeue()
			queues[queueIdx].credit -= serviceRate * 0.001
			if queues[queueIdx].credit < 0 {
				queues[queueIdx].credit = 0.0
			}
			fmt.Printf("Packet dequeued, size: %d, weight: %.1f, flow_id: %d, credit: %.2f\n",
				p.size, weights[queueIdx], p.flowId, queues[queueIdx].credit)
		}
	}

	fmt.Println("Final queue length: 0")
	fmt.Printf("Packets dropped: %d\n", dropped)
	fmt.Println("Final queue: empty")
}

func main() {
	rand.Seed(time.Now().UnixNano())
	packets := make([]Packet, 200)
	for i := range packets {
		packets[i] = Packet{
			size:   rand.Intn(100) + 1,
			weight: rand.Intn(5) + 1,
			flowId: rand.Intn(5),
		}
	}
	simulateCBFQ(packets, 100, 1000, 1.0)
}
