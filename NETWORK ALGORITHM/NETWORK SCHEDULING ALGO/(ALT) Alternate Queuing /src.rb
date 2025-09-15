# altq.rb
class Packet
  attr_accessor :size, :priority, :flow_id
  def initialize(size, priority, flow_id)
    @size = size
    @priority = priority
    @flow_id = flow_id
  end
end

class Queue
  attr_reader :items, :capacity, :total_bandwidth
  def initialize(capacity, total_bandwidth)
    @items = []
    @capacity = capacity
    @total_bandwidth = total_bandwidth
  end

  def enqueue(p)
    return false if @items.length >= @capacity
    @items << p
    true
  end

  def dequeue
    @items.shift
  end

  def size
    @items.length
  end
end

class PRIQ
  def initialize(num_queues, capacity, bandwidth)
    @queues = Array.new(num_queues) { Queue.new(capacity / num_queues, bandwidth / num_queues) }
  end

  def enqueue(p)
    pri = p.priority % @queues.length
    if @queues[pri].enqueue(p)
      puts "Packet enqueued, size: #{p.size}, priority: #{pri}, flow_id: #{p.flow_id}"
    else
      puts "Queue full, packet dropped, size: #{p.size}"
    end
  end

  def dequeue_all
    (@queues.length - 1).downto(0) do |pri|
      while @queues[pri].size > 0
        p = @queues[pri].dequeue
        puts "Packet dequeued, size: #{p.size}, priority: #{pri}, flow_id: #{p.flow_id}"
      end
    end
  end
end

class CoDelQueue
  def initialize(capacity, bandwidth, target, interval)
    @q = Queue.new(capacity, bandwidth)
    @target_delay = target
    @interval = interval
    @first_above_time = 0.0
    @drop_next = Float::INFINITY
    @drop_count = 0
  end

  def enqueue(p, current_time)
    if @q.enqueue(p)
      puts "Packet enqueued, size: #{p.size}, priority: #{p.priority}, flow_id: #{p.flow_id}"
      process_queue(current_time)
      true
    else
      puts "Queue full, packet dropped, size: #{p.size}"
      false
    end
  end

  def process_queue(current_time)
    while @q.size > 0
      sojourn_time = current_time # Simplified
      if sojourn_time < @target_delay || @q.size <= 4
        p = @q.dequeue
        puts "Packet dequeued, size: #{p.size}, priority: #{p.priority}, flow_id: #{p.flow_id}"
        @first_above_time = 0.0
        @drop_next = Float::INFINITY
        @drop_count = 0
      elsif @first_above_time == 0.0
        @first_above_time = current_time + @interval
        @drop_next = @first_above_time
        p = @q.dequeue
        puts "Packet dequeued, size: #{p.size}, priority: #{p.priority}, flow_id: #{p.flow_id}"
      elsif current_time >= @drop_next
        p = @q.dequeue
        puts "Packet dropped, size: #{p.size}, priority: #{p.priority}, flow_id: #{p.flow_id}"
        @drop_count += 1
        @drop_next = current_time + @interval / Math.sqrt(@drop_count)
      else
        p = @q.dequeue
        puts "Packet dequeued, size: #{p.size}, priority: #{p.priority}, flow_id: #{p.flow_id}"
        @drop_count = 0
      end
    end
  end

  def size
    @q.size
  end
end

class CBQ
  def initialize(num_nodes, capacity, bandwidth)
    @nodes = Array.new(num_nodes) { Queue.new(capacity / num_nodes, bandwidth / num_nodes) }
  end

  def enqueue(p)
    node_idx = p.flow_id % @nodes.length
    if @nodes[node_idx].enqueue(p)
      puts "Packet enqueued, size: #{p.size}, priority: #{p.priority}, flow_id: #{p.flow_id}"
    else
      puts "Queue full, packet dropped, size: #{p.size}"
    end
  end

  def dequeue_all
    @nodes.each_with_index do |node, node_idx|
      while node.size > 0
        p = node.dequeue
        puts "Packet dequeued, size: #{p.size}, priority: #{p.priority}, flow_id: #{p.flow_id}"
      end
    end
  end
end

class FairQ
  def initialize(num_flows, capacity, bandwidth)
    @flow_queues = Array.new(num_flows) { Queue.new(capacity / num_flows, bandwidth / num_flows) }
  end

  def enqueue(p)
    flow = p.flow_id % @flow_queues.length
    if @flow_queues[flow].enqueue(p)
      puts "Packet enqueued, size: #{p.size}, priority: #{p.priority}, flow_id: #{flow}"
    else
      puts "Queue full, packet dropped, size: #{p.size}"
    end
  end

  def dequeue_all
    @flow_queues.each_with_index do |flow_queue, flow|
      while flow_queue.size > 0
        p = flow_queue.dequeue
        puts "Packet dequeued, size: #{p.size}, priority: #{p.priority}, flow_id: #{flow}"
      end
    end
  end
end

class HFSC
  def initialize(num_nodes, capacity, bandwidth)
    @nodes = Array.new(num_nodes) { Queue.new(capacity / num_nodes, bandwidth / num_nodes) }
  end

  def enqueue(p)
    node_idx = p.priority % @nodes.length
    if @nodes[node_idx].enqueue(p)
      puts "Packet enqueued, size: #{p.size}, priority: #{p.priority}, flow_id: #{p.flow_id}"
    else
      puts "Queue full, packet dropped, size: #{p.size}"
    end
  end

  def dequeue_all
    @nodes.each_with_index do |node, node_idx|
      while node.size > 0
        p = node.dequeue
        puts "Packet dequeued, size: #{p.size}, priority: #{p.priority}, flow_id: #{p.flow_id}"
      end
    end
  end
end

def simulate_altq(packets, capacity, bandwidth)
  puts "=== ALTQ Schedulers Simulation ==="

  puts "\n=== PRIQ Scheduler ==="
  puts "Initial queue: empty"
  priq = PRIQ.new(16, capacity, bandwidth)
  dropped = 0
  packets.each do |p|
    priq.enqueue(p)
  end
  priq.dequeue_all
  puts "Final queue length: 0"
  puts "Packets dropped: #{dropped}"
  puts "Final queue: empty"

  puts "\n=== CoDel Scheduler ==="
  puts "Initial queue: empty"
  codel = CoDelQueue.new(capacity, bandwidth, 5.0, 100.0)
  current_time = 0.0
  packets.each do |p|
    codel.enqueue(p, current_time) || (dropped += 1)
    current_time += 1.0
  end
  puts "Final queue length: #{codel.size}"
  puts "Packets dropped: #{dropped}"
  puts "Final queue: empty"

  puts "\n=== CBQ Scheduler ==="
  puts "Initial queue: empty"
  cbq = CBQ.new(4, capacity, bandwidth)
  packets.each do |p|
    cbq.enqueue(p)
  end
  cbq.dequeue_all
  puts "Final queue length: 0"
  puts "Packets dropped: #{dropped}"
  puts "Final queue: empty"

  puts "\n=== FairQ Scheduler ==="
  puts "Initial queue: empty"
  fairq = FairQ.new(5, capacity, bandwidth)
  packets.each do |p|
    fairq.enqueue(p)
  end
  fairq.dequeue_all
  puts "Final queue length: 0"
  puts "Packets dropped: #{dropped}"
  puts "Final queue: empty"

  puts "\n=== HFSC Scheduler ==="
  puts "Initial queue: empty"
  hfsc = HFSC.new(4, capacity, bandwidth)
  packets.each do |p|
    hfsc.enqueue(p)
  end
  hfsc.dequeue_all
  puts "Final queue length: 0"
  puts "Packets dropped: #{dropped}"
  puts "Final queue: empty"
end

# Main execution
srand(42)
packets = 200.times.map { Packet.new(rand(1..100), rand(1..15), rand(1..5)) }
simulate_altq(packets, 100, 1000)
