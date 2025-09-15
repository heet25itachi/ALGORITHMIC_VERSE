# cbfq.jl
using Random

mutable struct Packet
    size::Int
    weight::Int
    flow_id::Int
end

mutable struct Queue
    items::Vector{Packet}
    capacity::Int
    credit::Float64
    credit_rate::Float64
    total_bandwidth::Int
end

Queue(capacity, credit_rate, total_bandwidth) = Queue(Packet[], capacity, 0.0, credit_rate, total_bandwidth)

function enqueue(q::Queue, p::Packet)
    if length(q.items) >= q.capacity
        return false
    end
    push!(q.items, p)
    true
end

function dequeue(q::Queue)
    popfirst!(q.items)
end

function update_credit!(q::Queue, delta_time)
    q.credit += q.credit_rate * delta_time
    q.credit = max(q.credit, 0.0)
end

function spend_credit!(q::Queue, amount)
    q.credit -= amount
    q.credit = max(q.credit, 0.0)
end

Base.length(q::Queue) = length(q.items)

function simulate_cbfq(packets, capacity, bandwidth, base_rate)
    weights = [1.0, 2.0, 3.0, 4.0, 5.0]
    queues = [Queue(capacity ÷ 5, w * base_rate, bandwidth ÷ 5) for w in weights]
    current_time = 0.0
    service_rate = 1000.0
    dropped = 0
    println("=== CBFQ Scheduler ===")
    println("Initial queue: empty")

    for p in packets
        queue_idx = mod(p.flow_id, 5) + 1
        update_credit!(queues[queue_idx], 0.001)

        if enqueue(queues[queue_idx], p)
            println("Packet enqueued, size: $(p.size), weight: $(weights[queue_idx]), flow_id: $(p.flow_id), credit: $(round(queues[queue_idx].credit, digits=2))")
        else
            println("Queue full, packet dropped, size: $(p.size)")
            dropped += 1
        end

        max_credit = -1.0
        max_idx = 0
        for (j, q) in enumerate(queues)
            if length(q) > 0 && q.credit > max_credit
                max_credit = q.credit
                max_idx = j
            end
        end
        if max_idx > 0
            p = dequeue(queues[max_idx])
            spend_credit!(queues[max_idx], service_rate * 0.001)
            println("Packet dequeued, size: $(p.size), weight: $(weights[max_idx]), flow_id: $(p.flow_id), credit: $(round(queues[max_idx].credit, digits=2))")
        end
        current_time += 0.001
    end

    for (queue_idx, q) in enumerate(queues)
        while length(q) > 0
            p = dequeue(q)
            spend_credit!(q, service_rate * 0.001)
            println("Packet dequeued, size: $(p.size), weight: $(weights[queue_idx]), flow_id: $(p.flow_id), credit: $(round(q.credit, digits=2))")
        end
    end

    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")
end

# Main execution
Random.seed!(42)
packets = [Packet(rand(1:100), rand(1:5), rand(0:4)) for _ in 1:200]
simulate_cbfq(packets, 100, 1000, 1.0)
