% Define Packet and Queue structures
classdef Packet
    properties
        size
        arrival_time
    end
    methods
        function obj = Packet(size)
            obj.size = size;
            obj.arrival_time = 0;
        end
    end
end

classdef Queue
    properties
        items
        capacity
    end
    methods
        function obj = Queue(capacity)
            obj.items = Packet.empty;
            obj.capacity = capacity;
        end
        function r = enqueue(obj, p)
            if length(obj.items) >= obj.capacity
                r = false;
                return
            end
            obj.items(end+1) = p;
            r = true;
        end
        function p = dequeue(obj)
            p = obj.items(1);
            obj.items(1) = [];
        end
        function p = peek(obj)
            p = obj.items(1);
        end
        function s = size(obj)
            s = length(obj.items);
        end
    end
end

% Simulate PIE algorithm
function simulate_pie(packets, target, update_interval, alpha, beta, max_drop_prob, max_burst, capacity)
    q = Queue(capacity);
    current_time = 0;
    last_update = 0;
    drop_prob = 0;
    prev_delay = 0;
    burst_time = max_burst;
    dropped = 0;
    fprintf('Initial queue: empty\n');
    rng(42);

    for i = 1:length(packets)
        packets(i).arrival_time = current_time;
        delay = 0;
        if q.size() > 0
            delay = current_time - q.peek().arrival_time;
        end

        if current_time - last_update >= update_interval
            error = delay - target;
            drop_prob = drop_prob + alpha * error + beta * (delay - prev_delay);
            drop_prob = max(0, min(max_drop_prob, drop_prob));
            prev_delay = delay;
            last_update = current_time;
            if delay > target
                burst_time = 0;
            elseif burst_time < max_burst
                burst_time = burst_time + update_interval;
            end
        end

        drop = burst_time < max_burst && delay > target && rand < drop_prob;

        if drop
            fprintf('Packet dropped, size: %d, queue delay: %.2f, drop prob: %.4f\n', packets(i).size, delay, drop_prob);
            dropped = dropped + 1;
        elseif q.enqueue(packets(i))
            fprintf('Packet enqueued, size: %d, queue delay: %.2f, drop prob: %.4f\n', packets(i).size, delay, drop_prob);
            deq_p = q.dequeue();
            fprintf('Packet dequeued, size: %d, queue delay: %.2f\n', deq_p.size, delay);
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
        current_time = current_time + 1;
    end

    while q.size() > 0
        delay = current_time - q.peek().arrival_time;
        if current_time - last_update >= update_interval
            error = delay - target;
            drop_prob = drop_prob + alpha * error + beta * (delay - prev_delay);
            drop_prob = max(0, min(max_drop_prob, drop_prob));
            prev_delay = delay;
            last_update = current_time;
            if delay > target
                burst_time = 0;
            elseif burst_time < max_burst
                burst_time = burst_time + update_interval;
            end
        end
        deq_p = q.dequeue();
        fprintf('Packet dequeued, size: %d, queue delay: %.2f\n', deq_p.size, delay);
        current_time = current_time + 1;
    end

    fprintf('Final queue length: %d\n', q.size());
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: empty\n');
end

% Main
rng(42);
packets = Packet.empty;
for i = 1:200
    packets(i) = Packet(randi([1, 100]));
end
fprintf('=== PIE ===\n');
simulate_pie(packets, 15, 30, 0.125, 1.25, 0.1, 150, 100);
