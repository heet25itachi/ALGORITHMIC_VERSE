using Random

struct Packet
    size::Int
    arrival_time::Float64
end

mutable struct CoDelQueue
    items::Vector{Packet}
    capacity::Int
    first_above_time::Float64
    drop_next::Float64
    drop_count::Int
end

CoDelQueue(capacity::Int) = CoDelQueue(Packet[], capacity, 0.0, Inf, 0)

function enqueue!(q::CoDelQueue, p::Packet)
    if length(q.items) >= q.capacity
        println("Queue full, packet dropped, size: ", p.size)
        return false
    end
    push!(q.items, p)
    println("Packet enqueued, size: ", p.size)
    true
end

function dequeue!(q::CoDelQueue, current_time::Float64, target::Float64, interval::Float64)
    while !isempty(q.items)
        p = q.items[1]
        sojourn_time = current_time - p.arrival_time

        if sojourn_time < target || length(q.items) <= 4
            q.first_above_time = 0.0
            q.drop_next = Inf
            popfirst!(q.items)
            println("Packet dequeued, size: ", p.size, ", sojourn time: ", round(sojourn_time, digits=2))
            q.drop_count = 0
        elseif q.first_above_time == 0.0
            q.first_above_time = current_time + interval
            q.drop_next = q.first_above_time
            popfirst!(q.items)
            println("Packet dequeued, size: ", p.size, ", sojourn time: ", round(sojourn_time, digits=2))
        elseif current_time >= q.drop_next
            popfirst!(q.items)
            println("Packet dropped, size: ", p.size, ", sojourn time: ", round(sojourn_time, digits=2))
            q.drop_count += 1
            q.drop_next = current_time + interval / sqrt(q.drop_count)
        else
            popfirst!(q.items)
            println("Packet dequeued, size: ", p.size, ", sojourn time: ", round(sojourn_time, digits=2))
            q.drop_count = 0
        end
    end
end

function simulate_codel(packets, target, interval, capacity)
    q = CoDelQueue(capacity)
    current_time = 0.0
    dropped = 0
    println("Initial queue: empty")

    for p in packets
        packet = Packet(p.size, current_time)
        if enqueue!(q, packet)
            dequeue!(q, current_time, target, interval)
        else
            dropped += 1
        end
        current_time += 1.0
    end

    dequeue!(q, current_time, target, interval)

    println("Final queue length: ", queue_size(q))
    println("Packets dropped: ", dropped)
    println("Final queue: empty")
end

Random.seed!(42)
packets = [Packet(rand(1:100), 0.0) for _ in 1:200]
simulate_codel(packets, 5.0, 100.0, 100)
