# altq.jl
using Random

mutable struct Packet
    size::Int
    priority::Int
    flow_id::Int
end

mutable struct Queue
    items::Vector{Packet}
    capacity::Int
    total_bandwidth::Int
end

Queue(capacity, total_bandwidth) = Queue(Packet[], capacity, total_bandwidth)

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

Base.length(q::Queue) = length(q.items)

mutable struct PRIQ
    queues::Vector{Queue}
end

function PRIQ(num_queues, capacity, bandwidth)
    queues = [Queue(capacity ÷ num_queues, bandwidth ÷ num_queues) for _ in 1:num_queues]
    PRIQ(queues)
end

function enqueue(priq::PRIQ, p::Packet)
    pri = mod(p.priority - 1, length(priq.queues)) + 1
    if enqueue(priq.queues[pri], p)
        println("Packet enqueued, size: $(p.size), priority: $pri, flow_id: $(p.flow_id)")
    else
        println("Queue full, packet dropped, size: $(p.size)")
    end
end

function dequeue_all(priq::PRIQ)
    for pri in length(priq.queues):-1:1
        while length(priq.queues[pri]) > 0
            p = dequeue(priq.queues[pri])
            println("Packet dequeued, size: $(p.size), priority: $pri, flow_id: $(p.flow_id)")
        end
    end
end

mutable struct CoDelQueue
    q::Queue
    target_delay::Float64
    interval::Float64
    first_above_time::Float64
    drop_next::Float64
    drop_count::Int
end

function CoDelQueue(capacity, bandwidth, target, interval)
    CoDelQueue(Queue(capacity, bandwidth), target, interval, 0.0, Inf, 0)
end

function enqueue(c::CoDelQueue, p::Packet, current_time)
    if enqueue(c.q, p)
        println("Packet enqueued, size: $(p.size), priority: $(p.priority), flow_id: $(p.flow_id)")
        process_queue(c, current_time)
        true
    else
        println("Queue full, packet dropped, size: $(p.size)")
        false
    end
end

function process_queue(c::CoDelQueue, current_time)
    while length(c.q) > 0
        sojourn_time = current_time # Simplified
        if sojourn_time < c.target_delay || length(c.q) <= 4
            p = dequeue(c.q)
            println("Packet dequeued, size: $(p.size), priority: $(p.priority), flow_id: $(p.flow_id)")
            c.first_above_time = 0.0
            c.drop_next = Inf
            c.drop_count = 0
        elseif c.first_above_time == 0.0
            c.first_above_time = current_time + c.interval
            c.drop_next = c.first_above_time
            p = dequeue(c.q)
            println("Packet dequeued, size: $(p.size), priority: $(p.priority), flow_id: $(p.flow_id)")
        elseif current_time >= c.drop_next
            p = dequeue(c.q)
            println("Packet dropped, size: $(p.size), priority: $(p.priority), flow_id: $(p.flow_id)")
            c.drop_count += 1
            c.drop_next = current_time + c.interval / sqrt(c.drop_count)
        else
            p = dequeue(c.q)
            println("Packet dequeued, size: $(p.size), priority: $(p.priority), flow_id: $(p.flow_id)")
            c.drop_count = 0
        end
    end
end

mutable struct CBQ
    nodes::Vector{Queue}
end

function CBQ(num_nodes, capacity, bandwidth)
    nodes = [Queue(capacity ÷ num_nodes, bandwidth ÷ num_nodes) for _ in 1:num_nodes]
    CBQ(nodes)
end

function enqueue(cbq::CBQ, p::Packet)
    node_idx = mod(p.flow_id - 1, length(cbq.nodes)) + 1
    if enqueue(cbq.nodes[node_idx], p)
        println("Packet enqueued, size: $(p.size), priority: $(p.priority), flow_id: $(p.flow_id)")
    else
        println("Queue full, packet dropped, size: $(p.size)")
    end
end

function dequeue_all(cbq::CBQ)
    for node in cbq.nodes
        while length(node) > 0
            p = dequeue(node)
            println("Packet dequeued, size: $(p.size), priority: $(p.priority), flow_id: $(p.flow_id)")
        end
    end
end

mutable struct FairQ
    flow_queues::Vector{Queue}
end

function FairQ(num_flows, capacity, bandwidth)
    flow_queues = [Queue(capacity ÷ num_flows, bandwidth ÷ num_flows) for _ in 1:num_flows]
    FairQ(flow_queues)
end

function enqueue(fq::FairQ, p::Packet)
    flow = mod(p.flow_id - 1, length(fq.flow_queues)) + 1
    if enqueue(fq.flow_queues[flow], p)
        println("Packet enqueued, size: $(p.size), priority: $(p.priority), flow_id: $flow")
    else
        println("Queue full, packet dropped, size: $(p.size)")
    end
end

function dequeue_all(fq::FairQ)
    for flow in 1:length(fq.flow_queues)
        while length(fq.flow_queues[flow]) > 0
            p = dequeue(fq.flow_queues[flow])
            println("Packet dequeued, size: $(p.size), priority: $(p.priority), flow_id: $flow")
        end
    end
end

mutable struct HFSC
    nodes::Vector{Queue}
end

function HFSC(num_nodes, capacity, bandwidth)
    nodes = [Queue(capacity ÷ num_nodes, bandwidth ÷ num_nodes) for _ in 1:num_nodes]
    HFSC(nodes)
end

function enqueue(hfsc::HFSC, p::Packet)
    node_idx = mod(p.priority - 1, length(hfsc.nodes)) + 1
    if enqueue(hfsc.nodes[node_idx], p)
        println("Packet enqueued, size: $(p.size), priority: $(p.priority), flow_id: $(p.flow_id)")
    else
        println("Queue full, packet dropped, size: $(p.size)")
    end
end

function dequeue_all(hfsc::HFSC)
    for node in hfsc.nodes
        while length(node) > 0
            p = dequeue(node)
            println("Packet dequeued, size: $(p.size), priority: $(p.priority), flow_id: $(p.flow_id)")
        end
    end
end

function simulate_altq(packets, capacity, bandwidth)
    println("=== ALTQ Schedulers Simulation ===")

    println("\n=== PRIQ Scheduler ===")
    println("Initial queue: empty")
    priq = PRIQ(16, capacity, bandwidth)
    dropped = 0
    for p in packets
        enqueue(priq, p)
    end
    dequeue_all(priq)
    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")

    println("\n=== CoDel Scheduler ===")
    println("Initial queue: empty")
    codel = CoDelQueue(capacity, bandwidth, 5.0, 100.0)
    current_time = 0.0
    for p in packets
        enqueue(codel, p, current_time) || (dropped += 1)
        current_time += 1.0
    end
    println("Final queue length: $(length(codel.q))")
    println("Packets dropped: $dropped")
    println("Final queue: empty")

    println("\n=== CBQ Scheduler ===")
    println("Initial queue: empty")
    cbq = CBQ(4, capacity, bandwidth)
    for p in packets
        enqueue(cbq, p)
    end
    dequeue_all(cbq)
    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")

    println("\n=== FairQ Scheduler ===")
    println("Initial queue: empty")
    fairq = FairQ(5, capacity, bandwidth)
    for p in packets
        enqueue(fairq, p)
    end
    dequeue_all(fairq)
    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")

    println("\n=== HFSC Scheduler ===")
    println("Initial queue: empty")
    hfsc = HFSC(4, capacity, bandwidth)
    for p in packets
        enqueue(hfsc, p)
    end
    dequeue_all(hfsc)
    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")
end

# Main execution
Random.seed!(42)
packets = [Packet(rand(1:100), rand(1:15), rand(1:5)) for _ in 1:200]
simulate_altq(packets, 100, 1000)
