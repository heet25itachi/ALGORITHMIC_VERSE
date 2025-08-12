% Define Packet and Queue structures
classdef Packet
    properties
        size
        flow_id
    end
    methods
        function obj = Packet(size, flow_id)
            obj.size = size;
            obj.flow_id = flow_id;
        end
    end
end

classdef Bin
    properties
        p
        last_update
    end
    methods
        function obj = Bin
            obj.p = 0;
            obj.last_update = 0;
        end
    end
end

classdef SFB
    properties
        bins
        L
        N
    end
    methods
        function obj = SFB(l, n)
            obj.bins = cell(l * n, 1);
            for i = 1:l*n
                obj.bins{i} = Bin;
            end
            obj.L = l;
            obj.N = n;
        end
        function marked = drop(obj, flow_id, current_time, d1, d2, freeze_time, queue_length, capacity)
            marked = 1;
            for l = 1:obj.L
                bin_idx = (l-1) * obj.N + mod(flow_id + (l-1), obj.N) + 1; # Simple hash
                bin = obj.bins{bin_idx};
                if current_time - bin.last_update >= freeze_time
                    if queue_length == 0
                        bin.p = max(bin.p - d2, 0);
                    elseif queue_length >= capacity
                        bin.p = bin.p + d1;
                    end
                    bin.last_update = current_time;
                end
                if bin.p < 1.0
                    marked = 0;
                    break
                end
            end
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
            obj.items = obj.items(2:end);
        end
        function p = peek(obj)
            p = obj.items(1);
        end
        function s = size(obj)
            s = length(obj.items);
        end
    end
end

% Simulate SFB algorithm
function simulate_sfb(packets, d1, d2, freeze_time, capacity, l, n)
    q = Queue(capacity);
    sfb = SFB(l, n);
    current_time = 0;
    dropped = 0;
    fprintf('Initial queue: empty\n');

    rng(42);

    for i = 1:length(packets)
        if sfb.drop(packets(i).flow_id, current_time, d1, d2, freeze_time, q.size, capacity)
            fprintf('Packet dropped, size: %d, flow_id: %d\n', packets(i).size, packets(i).flow_id);
            dropped = dropped + 1;
        elseif q.enqueue(packets(i))
            fprintf('Packet enqueued, size: %d, flow_id: %d\n', packets(i).size, packets(i).flow_id);
            # Immediate dequeue for simulation
            deq_p = q.dequeue;
            fprintf('Packet dequeued, size: %d, flow_id: %d\n', deq_p.size, deq_p.flow_id);
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
        current_time = current_time + 1;
    end

    while q.size > 0
        p = q.dequeue;
        fprintf('Packet dequeued, size: %d, flow_id: %d\n', p.size, p.flow_id);
        current_time = current_time + 1;
    end

    fprintf('Final queue length: %d\n', q.size);
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: empty\n');
end

% Main
rng(42);
packets = Packet.empty;
for i = 1:200
    packets(i) = Packet(randi([1, 100]), randi([1, 20]));
end
fprintf('=== SFB ===\n');
simulate_sfb(packets, 0.0002, 0.00005, 100, 100, 2, 4);
