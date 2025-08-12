class Packet
  attr_accessor :size, :arrival_time
  def initialize(size, arrival_time = 0.0)
    @size = size
    @arrival_time = arrival_time
  end
end

class CoDelQueue
  attr_accessor :first_above_time, :drop_next, :drop_count
  def initialize(capacity)
    @items = []
    @capacity = capacity
    @first_above_time = 0.0
    @drop_next = Float::INFINITY
    @drop_count = 0
  end

  def enqueue(p)
    if @items.size >= @capacity
        puts "Queue full, packet dropped, size: #{p.size}"
        return false
    end
    @items << p
    puts "Packet enqueued, size: #{p.size}"
    true
  end

  def dequeue(current_time, target, interval)
    while !@items.empty?
        p = @items[0]
        sojourn_time = current_time - p.arrival_time

        if sojourn_time < target || @items.size <= 4
            @first_above_time = 0.0
            @drop_next = Float::INFINITY
            @items.shift
            puts "Packet dequeued, size: #{p.size}, sojourn time: #{'%.2f' % sojourn_time}"
            @drop_count = 0
        elsif @first_above_time == 0.0
            @first_above_time = current_time + interval
            @drop_next = @first_above_time
            @items.shift
            puts "Packet dequeued, size: #{p.size}, sojourn time: #{'%.2f' % sojourn_time}"
        elsif current_time >= @drop_next
            @items.shift
            puts "Packet dropped, size: #{p.size}, sojourn time: #{'%.2f' % sojourn_time}"
            @drop_count += 1
            @drop_next = current_time + interval / Math.sqrt(@drop_count)
        else
            @items.shift
            puts "Packet dequeued, size: #{p.size}, sojourn time: #{'%.2f' % sojourn_time}"
            @drop_count = 0
        end
    end
end

  def size
    @items.size
  end
end

def simulate_codel(packets, target, interval, capacity)
    q = CoDelQueue.new(capacity)
    current_time = 0.0
    dropped = 0
    puts "Initial queue: empty"

    packets.each do |p|
        packet = Packet.new(p.size, current_time)
        if q.enqueue(packet)
            q.dequeue(current_time, target, interval)
        else
            dropped += 1
        end
        current_time += 1.0
    end

    q.dequeue(current_time, target, interval)

    puts "Final queue length: #{q.size}"
    puts "Packets dropped: #{dropped}"
    puts "Final queue: empty"
end

# Main
packets = Array.new(200) { Packet.new(rand(1..100)) }
simulate_codel(packets, 5.0, 100.0, 100)
