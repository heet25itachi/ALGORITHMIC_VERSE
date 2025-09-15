% altq.m
function altq()
    rng(42); % Set seed for reproducibility
    packets = struct('size', num2cell(randi([1, 100], 200, 1)), ...
                     'priority', num2cell(randi([1, 15], 200, 1)), ...
                     'flow_id', num2cell(randi([1, 5], 200, 1)));
    capacity = 100;
    bandwidth = 1000;
    
    fprintf('=== ALTQ Schedulers Simulation ===\n');
    simulate_priq(packets, capacity, bandwidth);
    simulate_codel(packets, capacity, bandwidth);
    simulate_cbq(packets, capacity, bandwidth);
    simulate_fairq(packets, capacity, bandwidth);
    simulate_hfsc(packets, capacity, bandwidth);
end

function simulate_priq(packets, capacity, bandwidth)
    num_queues = 16;
    queues = cell(num_queues, 1);
    for i = 1:num_queues
        queues{i} = struct('items', {}, 'capacity', capacity/num_queues, 'bandwidth', bandwidth/num_queues);
    end
    dropped = 0;
    fprintf('\n=== PRIQ Scheduler ===\n');
    fprintf('Initial queue: empty\n');
    
    for i = 1:length(packets)
        pri = mod(packets(i).priority - 1, num_queues) + 1;
        if length(queues{pri}.items) < queues{pri}.capacity
            queues{pri}.items{end+1} = packets(i);
            fprintf('Packet enqueued, size: %d, priority: %d, flow_id: %d\n', ...
                    packets(i).size, pri, packets(i).flow_id);
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
    end
    
    for pri = num_queues:-1:1
        while ~isempty(queues{pri}.items)
            p = queues{pri}.items{1};
            queues{pri}.items(1) = [];
            fprintf('Packet dequeued, size: %d, priority: %d, flow_id: %d\n', ...
                    p.size, pri, p.flow_id);
        end
    end
    
    fprintf('Final queue length: 0\n');
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: empty\n');
end

function simulate_codel(packets, capacity, bandwidth)
    queue = struct('items', {}, 'capacity', capacity, 'bandwidth', bandwidth);
    target_delay = 5.0;
    interval = 100.0;
    first_above_time = 0.0;
    drop_next = Inf;
    drop_count = 0;
    dropped = 0;
    current_time = 0.0;
    fprintf('\n=== CoDel Scheduler ===\n');
    fprintf('Initial queue: empty\n');
    
    for i = 1:length(packets)
        if length(queue.items) < queue.capacity
            queue.items{end+1} = packets(i);
            fprintf('Packet enqueued, size: %d, priority: %d, flow_id: %d\n', ...
                    packets(i).size, packets(i).priority, packets(i).flow_id);
            while ~isempty(queue.items)
                sojourn_time = current_time; % Simplified
                if sojourn_time < target_delay || length(queue.items) <= 4
                    p = queue.items{1};
                    queue.items(1) = [];
                    fprintf('Packet dequeued, size: %d, priority: %d, flow_id: %d\n', ...
                            p.size, p.priority, p.flow_id);
                    first_above_time = 0.0;
                    drop_next = Inf;
                    drop_count = 0;
                elseif first_above_time == 0.0
                    first_above_time = current_time + interval;
                    drop_next = first_above_time;
                    p = queue.items{1};
                    queue.items(1) = [];
                    fprintf('Packet dequeued, size: %d, priority: %d, flow_id: %d\n', ...
                            p.size, p.priority, p.flow_id);
                elseif current_time >= drop_next
                    p = queue.items{1};
                    queue.items(1) = [];
                    fprintf('Packet dropped, size: %d, priority: %d, flow_id: %d\n', ...
                            p.size, p.priority, p.flow_id);
                    drop_count = drop_count + 1;
                    drop_next = current_time + interval / sqrt(drop_count);
                else
                    p = queue.items{1};
                    queue.items(1) = [];
                    fprintf('Packet dequeued, size: %d, priority: %d, flow_id: %d\n', ...
                            p.size, p.priority, p.flow_id);
                    drop_count = 0;
                end
            end
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
        current_time = current_time + 1.0;
    end
    
    fprintf('Final queue length: %d\n', length(queue.items));
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: empty\n');
end

function simulate_cbq(packets, capacity, bandwidth)
    num_nodes = 4;
    nodes = cell(num_nodes, 1);
    for i = 1:num_nodes
        nodes{i} = struct('items', {}, 'capacity', capacity/num_nodes, 'bandwidth', bandwidth/num_nodes);
    end
    dropped = 0;
    fprintf('\n=== CBQ Scheduler ===\n');
    fprintf('Initial queue: empty\n');
    
    for i = 1:length(packets)
        node_idx = mod(packets(i).flow_id - 1, num_nodes) + 1;
        if length(nodes{node_idx}.items) < nodes{node_idx}.capacity
            nodes{node_idx}.items{end+1} = packets(i);
            fprintf('Packet enqueued, size: %d, priority: %d, flow_id: %d\n', ...
                    packets(i).size, packets(i).priority, packets(i).flow_id);
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
    end
    
    for node_idx = 1:num_nodes
        while ~isempty(nodes{node_idx}.items)
            p = nodes{node_idx}.items{1};
            nodes{node_idx}.items(1) = [];
            fprintf('Packet dequeued, size: %d, priority: %d, flow_id: %d\n', ...
                    p.size, p.priority, p.flow_id);
        end
    end
    
    fprintf('Final queue length: 0\n');
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: empty\n');
end

function simulate_fairq(packets, capacity, bandwidth)
    num_flows = 5;
    flow_queues = cell(num_flows, 1);
    for i = 1:num_flows
        flow_queues{i} = struct('items', {}, 'capacity', capacity/num_flows, 'bandwidth', bandwidth/num_flows);
    end
    dropped = 0;
    fprintf('\n=== FairQ Scheduler ===\n');
    fprintf('Initial queue: empty\n');
    
    for i = 1:length(packets)
        flow = mod(packets(i).flow_id - 1, num_flows) + 1;
        if length(flow_queues{flow}.items) < flow_queues{flow}.capacity
            flow_queues{flow}.items{end+1} = packets(i);
            fprintf('Packet enqueued, size: %d, priority: %d, flow_id: %d\n', ...
                    packets(i).size, packets(i).priority, flow);
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
    end
    
    for flow = 1:num_flows
        while ~isempty(flow_queues{flow}.items)
            p = flow_queues{flow}.items{1};
            flow_queues{flow}.items(1) = [];
            fprintf('Packet dequeued, size: %d, priority: %d, flow_id: %d\n', ...
                    p.size, p.priority, flow);
        end
    end
    
    fprintf('Final queue length: 0\n');
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: empty\n');
end

function simulate_hfsc(packets, capacity, bandwidth)
    num_nodes = 4;
    nodes = cell(num_nodes, 1);
    for i = 1:num_nodes
        nodes{i} = struct('items', {}, 'capacity', capacity/num_nodes, 'bandwidth', bandwidth/num_nodes);
    end
    dropped = 0;
    fprintf('\n=== HFSC Scheduler ===\n');
    fprintf('Initial queue: empty\n');
    
    for i = 1:length(packets)
        node_idx = mod(packets(i).priority - 1, num_nodes) + 1;
        if length(nodes{node_idx}.items) < nodes{node_idx}.capacity
            nodes{node_idx}.items{end+1} = packets(i);
            fprintf('Packet enqueued, size: %d, priority: %d, flow_id: %d\n', ...
                    packets(i).size, packets(i).priority, packets(i).flow_id);
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
    end
    
    for node_idx = 1:num_nodes
        while ~isempty(nodes{node_idx}.items)
            p = nodes{node_idx}.items{1};
            nodes{node_idx}.items(1) = [];
            fprintf('Packet dequeued, size: %d, priority: %d, flow_id: %d\n', ...
                    p.size, p.priority, p.flow_id);
        end
    end
    
    fprintf('Final queue length: 0\n');
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: empty\n');
end
