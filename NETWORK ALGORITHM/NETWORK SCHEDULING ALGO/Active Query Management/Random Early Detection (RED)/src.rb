# Define Packet and Queue classes
class Packet
    attr_accessor :size
    def initialize(size)
        @size = size
    end
end

class Queue
    attr_reader :items, :capacity
    def initialize(capacity)
        @items = []
        @capacity = capacity
    end
    def enqueue(p)
        return false if @items.size >= @capacity
        @items << p
        true
    end
    def size
        @items.size
    end
    def print_queue
        puts "Final queue: #{@items.map(&:size).join(' ')}"
    end
end

# Simulate RED algorithm
def simulate_red(packets, min_th, max_th, w_q, max_p, capacity)
    q = Queue.new(capacity)
    avg, count, dropped = 0.0, 0.0, 0.0
    puts "Initial queue: empty"
    packets.each do |p|
        avg = q.size == 0 ? 0.0 : (1 - w_q) * avg + w_q * q.size
        drop = if avg < min_th
                   false
               elsif avg >= max_th
                   true
               else
                   pb = max_p * (avg - min_th) / (max_th - min_th)
                   pa = pb / (1 - count * pb)
                   count += 1
                   rand < pa
               end
        if drop
            puts "Packet dropped, size: #{p.size}, avg queue length: %.2f, max_p: %.4f" % [avg, max_p]
            dropped += 1
        elsif q.enqueue(p)
            puts "Packet enqueued, size: #{p.size}, avg queue length: %.2f, max_p: %.4f" % [avg, max_p]
            count = 0
        else
            puts "Queue full, packet dropped, size: #{p.size}"
            dropped += 1
        end
    end
    puts "Final queue length: #{q.size}"
    puts
