% Define Packet and Queue structures
classdef Packet
    properties
        size
    end
    methods
        function obj = Packet(size)
            obj.size = size;
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
        function s = size(obj)
            s = length(obj.items);
        end
        function printQueue(obj)
            fprintf('Final queue: ');
            for i = 1:length(obj.items)
                fprintf('%d ', obj.items(i).size);
            end
            fprintf('\n');
        end
    end
end

% Simulate RED algorithm
function simulate_red(packets, min_th, max_th, w_q, max_p, capacity)
    q = Queue(capacity);
    avg = 0;
    count = 0;
    dropped = 0;
    fprintf('Initial queue: empty\n');
    for i = 1:length(packets)
        if q.size() == 0
            avg = 0;
        else
            avg = (1 - w_q) * avg + w_q * q.size();
        end
        if avg < min_th
            drop = false;
        elseif avg >= max_th
            drop = true;
        else
            pb = max_p * (avg - min_th) / (max_th - min_th);
            pa = pb / (1 - count * pb);
            count = count + 1;
            drop = rand() < pa;
        end
        if drop
            fprintf('Packet dropped, size: %d, avg queue length: %.2f, max_p: %.4f\n', packets(i).size, avg, max_p);
            dropped = dropped + 1;
        elseif q.enqueue(packets(i))
            fprintf('Packet enqueued, size: %d, avg queue length: %.2f, max_p: %.4f\n', packets(i).size, avg, max_p);
            count = 0;
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
    end
    fprintf('Final queue length: %d\n', q.size());
    fprintf('Packets dropped: %d\n', dropped);
    q.printQueue();
end

% Main
rng(42); % Set random seed
packets = Packet.empty;
for i = 1:200
    packets(i) = Packet(randi([1, 100]));
end
fprintf('=== RED ===\n');
simulate_red(packets, 20, 80, 0.002, 0.1, 100);
