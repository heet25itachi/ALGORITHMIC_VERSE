# Define Packet and Queue classes
class Packet
    attr_accessor :size, :arrival_time
    def initialize(size)
        @size = size
        @arrival_time = 0.0
    end
end

class Queue
    def initialize(capacity)
        @items = []
        @capacity = capacity
    end
    def enqueue(p)
        return false if @items.size >= @capacity
        @items << p
        true
    end
    def dequeue
        @items.shift
    end
    def peek
        @items[0]
    end
    def size
        @items.size
    end
end

# Simulate PIE algorithm
def simulate_pie(packets, target, update_interval, alpha, beta, max_drop_prob, max_burst, capacity)
    q = Queue.new(capacity)
    current_time, last_update, drop_prob, prev_delay, burst_time = 0.0, 0.0, 0.0, 0.0, max_burst
    dropped = 0
    puts "Initial queue: empty"
    srand(42)

    packets.each do |p|
        p.arrival_time = current_time
        delay = q.size == 0 ? 0.0 : current_time - q.peek.arrival_time

        if current_time - last_update >= update_interval
            error = delay - target
            drop_prob += alpha * error + beta * (delay - prev_delay)
            drop_prob = [0.0, [max_drop_prob, drop_prob].min].max
            prev_delay = delay
            last_update = current_time
            if delay > target
                burst_time = 0.0
            elsif burst_time < max_burst
                burst_time += update_interval
            end
        end

        drop = burst_time < max_burst && delay > target && rand < drop_prob

        if drop
            puts "Packet dropped, size: #{p.size}, queue delay: #{'%.2f' % delay}, drop prob: #{'%.4f' % drop_prob}"
            dropped += 1
        elsif q.enqueue(p)
            puts "Packet enqueued, size: #{p.size}, queue delay: #{'%.2f' % delay}, drop prob: #{'%.4f' % drop_prob}"
            deq_p = q.dequeue
            puts "Packet dequeued, size: #{deq_p.size}, queue delay: #{'%.2f' % delay}"
        else
            puts "Queue full, packet dropped, size: #{p.size}"
            dropped += 1
        end
        current_time += 1.0
    end

    while q.size > 0
        delay = current_time - q.peek.arrival_time
        if current_time - last_update >= update_interval
            error = delay - target
            drop_prob += alpha * error + beta * (delay - prev_delay)
            drop_prob = [0.0, [max_drop_prob, drop_prob].min].max
            prev_delay = delay
            last_update = current_time
            if delay > target
                burst_time = 0.0
            elsif burst_time < max_burst
                burst_time += update_interval
            end
        end
        deq_p = q.dequeue
        puts "Packet dequeued, size: #{deq_p.size}, queue delay: #{'%.2f' % delay}"
        current_time += 1.0
    end

    puts "Final queue length: #{q.size}"
    puts "Packets dropped: #{dropped}"
    puts "Final queue: empty"
end

# Main
packets = Array.new(200) { Packet.new(rand(1..100)) }
puts "=== PIE ==="
simulate_pie(packets, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100)
