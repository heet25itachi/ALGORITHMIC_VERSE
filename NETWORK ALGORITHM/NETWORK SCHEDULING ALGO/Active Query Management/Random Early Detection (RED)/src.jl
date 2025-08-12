using Random

# Define Packet and Queue structs
struct Packet
    size::Int
end

mutable struct Queue
    items::Vector{Packet}
    capacity::Int
end

Queue(capacity::Int) = Queue(Vector{Packet}(), capacity)

# Enqueue a packet
function enqueue!(q::Queue, p::Packet)
    if length(q.items) >= q.capacity
        return false
    end
    push!(q.items, p)
    true
end

# Get queue size
queue_size(q::Queue) = length(q.items)

# Print queue contents
function print_queue(q::Queue)
    println("Final queue: ", join([string(p.size) for p in q.items], " "))
end

# Simulate RED algorithm
function simulate_red(packets, min_th, max_th, w_q, max_p, capacity)
    q = Queue(capacity)
    avg, count, dropped = 0.0, 0.0, 0.0
    println("Initial queue: empty")
    for p in packets
        avg = queue_size(q) == 0 ? 0.0 : (1 - w_q) * avg + w_q * queue_size(q)
        drop = if avg < min_th
            false
        elseif avg >= max_th
            true
        else
            pb = max_p * (avg - min_th) / (max_th - min_th)
            pa = pb / (1 - count * pb)
            count += 1
            rand() < pa
        end
        if drop
            println("Packet dropped, size: $(p.size), avg queue length: $(round(avg, digits=2)), max_p: $(round(max_p, digits=4))")
            dropped += 1
        elseif enqueue!(q, p)
            println("Packet enqueued, size: $(p.size), avg queue length: $(round(avg, digits=2)), max_p: $(round(max_p, digits=4))")
            count = 0
        else
            println("Queue full, packet dropped, size: $(p.size)")
            dropped += 1
        end
    end
    println("Final queue length: $(queue_size(q))")
    println("Packets dropped: $(Int(dropped))")
    print_queue(q)
end

# Main
function main()
    Random.seed!(42)
    packets = [Packet(rand(1:100)) for _ in 1:200]
    println("=== RED ===")
    simulate_red(packets, 20.0, 80.0, 0.002, 0.1, 100)
end

main()
