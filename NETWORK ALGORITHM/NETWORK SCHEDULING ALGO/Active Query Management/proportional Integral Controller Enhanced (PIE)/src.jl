using Random

# Define Packet and Queue structs
struct Packet
    size::Int
    arrival_time::Float64
end

mutable struct Queue
    items::Vector{Packet}
    capacity::Int
end

Queue(capacity::Int) = Queue(Vector{Packet}(), capacity)

function enqueue!(q::Queue, p::Packet)
    if length(q.items) >= q.capacity
        return false
    end
    push!(q.items, p)
    true
end

function dequeue!(q::Queue)
    popfirst!(q.items)
end

function peek(q::Queue)
    q.items[1]
end

queue_size(q::Queue) = length(q.items)

# Simulate PIE algorithm
function simulate_pie(packets, target, update_interval, alpha, beta, max_drop_prob, max_burst, capacity)
    q = Queue(capacity)
    current_time, last_update, drop_prob, prev_delay, burst_time = 0.0, 0.0, 0.0, 0.0, max_burst
    dropped = 0
    println("Initial queue: empty")
    Random.seed!(42)

    for p in packets
        packet = Packet(p.size, current_time)
        delay = queue_size(q) == 0 ? 0.0 : current_time - peek(q).arrival_time

        if current_time - last_update >= update_interval
            error = delay - target
            drop_prob += alpha * error + beta * (delay - prev_delay)
            drop_prob = max(0.0, min(max_drop_prob, drop_prob))
            prev_delay = delay
            last_update = current_time
            if delay > target
                burst_time = 0.0
            elseif burst_time < max_burst
                burst_time += update_interval
            end
        end

        drop = burst_time < max_burst && delay > target && rand() < drop_prob

        if drop
            println("Packet dropped, size: $(p.size), queue delay: $(round(delay, digits=2)), drop prob: $(round(drop_prob, digits=4))")
            dropped += 1
        elseif enqueue!(q, packet)
            println("Packet enqueued, size: $(p.size), queue delay: $(round(delay, digits=2)), drop prob: $(round(drop_prob, digits=4))")
            deq_p = dequeue!(q)
            println("Packet dequeued, size: $(deq_p.size), queue delay: $(round(delay, digits=2))")
        else
            println("Queue full, packet dropped, size: $(p.size)")
            dropped += 1
        end
        current_time += 1.0
    end

    while queue_size(q) > 0
        delay = current_time - peek(q).arrival_time
        if current_time - last_update >= update_interval
            error = delay - target
            drop_prob += alpha * error + beta * (delay - prev_delay)
            drop_prob = max(0.0, min(max_drop_prob, drop_prob))
            prev_delay = delay
            last_update = current_time
            if delay > target
                burst_time = 0.0
            elseif burst_time < max_burst
                burst_time += update_interval
            end
        end
        deq_p = dequeue!(q)
        println("Packet dequeued, size: $(deq_p.size), queue delay: $(round(delay, digits=2))")
        current_time += 1.0
    end

    println("Final queue length: $(queue_size(q))")
    println("Packets dropped: $dropped")
    println("Final queue: empty")
end

# Main
Random.seed!(42)
packets = [Packet(rand(1:100), 0.0) for _ in 1:200]
println("=== PIE ===")
simulate_pie(packets, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100)
