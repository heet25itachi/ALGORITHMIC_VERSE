% cbfq.m
function cbfq()
    rng(42); % Set seed for reproducibility
    packets = struct('size', num2cell(randi([1, 100], 200, 1)), ...
                    'weight', num2cell(randi([1, 5], 200, 1)), ...
                    'flow_id', num2cell(randi([0, 4], 200, 1)));
    capacity = 100;
    bandwidth = 1000;
    base_rate = 1.0;
    
    fprintf('=== CBFQ Scheduler ===\n');
    fprintf('Initial queue: empty\n');
    
    weights = [1.0, 2.0, 3.0, 4.0, 5.0];
    queues = cell(5, 1);
    for i = 1:5
        queues{i} = struct('items', {}, 'capacity', capacity/5, 'credit', 0.0, ...
                          'credit_rate', weights(i)*base_rate, 'total_bandwidth', bandwidth/5);
    end
    current_time = 0.0;
    service_rate = 1000.0;
    dropped = 0;
    
    for i = 1:length(packets)
        queue_idx = mod(packets(i).flow_id, 5) + 1;
        queues{queue_idx}.credit = queues{queue_idx}.credit + queues{queue_idx}.credit_rate * 0.001;
        if queues{queue_idx}.credit < 0
            queues{queue_idx}.credit = 0.0;
        end
        
        if length(queues{queue_idx}.items) < queues{queue_idx}.capacity
            queues{queue_idx}.items{end+1} = packets(i);
            fprintf('Packet enqueued, size: %d, weight: %.1f, flow_id: %d, credit: %.2f\n', ...
                    packets(i).size, weights(queue_idx), packets(i).flow_id, queues{queue_idx}.credit);
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
        
        max_credit = -1.0;
        max_idx = 0;
        for j = 1:5
            if length(queues{j}.items) > 0 && queues{j}.credit > max_credit
                max_credit = queues{j}.credit;
                max_idx = j;
            end
        end
        if max_idx > 0
            p = queues{max_idx}.items{1};
            queues{max_idx}.items(1) = [];
            queues{max_idx}.credit = queues{max_idx}.credit - service_rate * 0.001;
            if queues{max_idx}.credit < 0
                queues{max_idx}.credit = 0.0;
            end
            fprintf('Packet dequeued, size: %d, weight: %.1f, flow_id: %d, credit: %.2f\n', ...
                    p.size, weights(max_idx), p.flow_id, queues{max_idx}.credit);
        end
        current_time = current_time + 0.001;
    end
    
    for queue_idx = 1:5
        while ~isempty(queues{queue_idx}.items)
            p = queues{queue_idx}.items{1};
            queues{queue_idx}.items(1) = [];
            queues{queue_idx}.credit = queues{queue_idx}.credit - service_rate * 0.001;
            if queues{queue_idx}.credit < 0
                queues{queue_idx}.credit = 0.0;
            end
            fprintf('Packet dequeued, size: %d, weight: %.1f, flow_id: %d, credit: %.2f\n', ...
                    p.size, weights(queue_idx), p.flow_id, queues{queue_idx}.credit);
        end
    end
    
    fprintf('Final queue length: 0\n');
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: empty\n');
end
