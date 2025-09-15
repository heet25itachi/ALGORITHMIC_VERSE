# cbfq.rb
class Packet
  attr_accessor :size, :weight, :flow_id
  def initialize(size, weight, flow_id)
    @size = size
    @weight = weight
    @flow_id = flow_id
  end
end

class Queue
  attr_reader :items, :capacity, :credit, :credit_rate, :total_bandwidth
  def initialize(capacity, credit_rate, total_bandwidth)
    @items = []
    @capacity = capacity
    @credit = 0.0
    @credit_rate = credit_rate
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

  def update_credit(delta_time)
    @credit += @credit_rate * delta_time
    @credit = [@credit, 0.0].max
  end

  def spend_credit(amount)
    @credit -= amount
    @credit = [@credit, 0.0].max
  end

  def size
    @items.length
  end
end

def simulate_cbfq(packets, capacity, bandwidth, base_rate)
  weights = [1.0, 2.0, 3.0, 4.0, 5.0]
  queues = weights.map { |w| Queue.new(capacity / 5, w * base_rate, bandwidth / 5) }
  current_time = 0.0
  service_rate = 1000.0
  dropped = 0
  puts "=== CBFQ Scheduler ==="
  puts "Initial queue: empty"

  packets.each do |p|
    queue_idx = p.flow_id % 5
    queues[queue_idx].update_credit(0.001)

    if queues[queue_idx].enqueue(p)
      puts "Packet enqueued, size: #{p.size}, weight: #{weights[queue_idx]}, flow_id: #{p.flow_id}, credit: #{queues[queue_idx].credit.round(2)}"
    else
      puts "Queue full, packet dropped, size: #{p.size}"
      dropped += 1
    end

    max_credit = -1.0
    max_idx = nil
    queues.each_with_index do |q, j|
      if q.size > 0 && q.credit > max_credit
        max_credit = q.credit
        max_idx = j
      end
    end
    if max_idx
      p = queues[max_idx].dequeue
      queues[max_idx].spend_credit(service_rate * 0.001)
      puts "Packet dequeued, size: #{p.size}, weight: #{weights[max_idx]}, flow_id: #{p.flow_id}, credit: #{queues[max_idx].credit.round(2)}"
    end
    current_time += 0.001
  end

  queues.each_with_index do |q, queue_idx|
    while q.size > 0
      p = q.dequeue
      q.spend_credit(service_rate * 0.001)
      puts "Packet dequeued, size: #{p.size}, weight: #{weights[queue_idx]}, flow_id: #{p.flow_id}, credit: #{q.credit.round(2)}"
    end
  end

  puts "Final queue length: 0"
  puts "Packets dropped: #{dropped}"
  puts "Final queue: empty"
end

# Main execution
srand(42)
packets = 200.times.map { Packet.new(rand(1..100), rand(1..5), rand(0..4)) }
simulate_cbfq(packets, 100, 1000, 1.0)
