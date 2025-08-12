function simulate_codel()
    % Packet structure
    packets = struct('size', num2cell(randi([1, 100], 1, 200)), 'arrival_time', num2cell(zeros(1, 200)));
    n = 200;
    capacity = 100;
    target = 5;
    interval = 100;

    % Queue initialization
    queue = struct('size', cell(1, capacity), 'arrival_time', cell(1, capacity));
    front = 1;
    rear = 0;
    length = 0;
    current_time = 0;
    first_above_time = 0;
    drop_next = inf;
    drop_count = 0;
    dropped = 0;

    fprintf('Initial queue: empty\n');

    for i = 1:n
        packets(i).arrival_time = current_time;
        if length >= capacity
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        else
            rear = rear + 1;
            queue(rear).size = packets(i).size;
            queue(rear).arrival_time = packets(i).arrival_time;
            length = length + 1;
            fprintf('Packet enqueued, size: %d\n', packets(i).size);
        end

        while length > 0
            sojourn_time = current_time - queue(front).arrival_time;

            if sojourn_time < target || length <= 4
                first_above_time = 0;
                drop_next = inf;
                fprintf('Packet dequeued, size: %d, sojourn time: %.2f\n', queue(front).size, sojourn_time);
                front = front + 1;
                length = length - 1;
                drop_count = 0;
            elseif first_above_time == 0
                first_above_time = current_time + interval;
                drop_next = first_above_time;
                fprintf('Packet dequeued, size: %d, sojourn time: %.2f\n', queue(front).size, sojourn_time);
                front = front + 1;
                length = length - 1;
            elseif current_time >= drop_next
                fprintf('Packet dropped, size: %d, sojourn time: %.2f\n', queue(front).size, sojourn_time);
                front = front + 1;
                length = length - 1;
                dropped = dropped + 1;
                drop_count = drop_count + 1;
                drop_next = current_time + interval / sqrt(drop_count);
            else
                fprintf('Packet dequeued, size: %d, sojourn time: %.2f\n', queue(front).size, sojourn_time);
                front = front + 1;
                length = length - 1;
                drop_count = 0;
            end
        end
        current_time = current_time + 1;
    end

    while length > 0
        sojourn_time = current_time - queue(front).arrival_time;

        if sojourn_time < target || length <= 4
            first_above_time = 0;
            drop_next = inf;
            fprintf('Packet dequeued, size: %d, sojourn time: %.2f\n', queue(front).size, sojourn_time);
            front = front + 1;
            length = length - 1;
            drop_count = 0;
        elseif first_above_time == 0
            first_above_time = current_time + interval;
            drop_next = first_above_time;
            fprintf('Packet dequeued, size: %d, sojourn time: %.2f\n', queue(front).size, sojourn_time);
            front = front + 1;
            length = length - 1;
        elseif current_time >= drop_next
            fprintf('Packet dropped, size: %d, sojourn time: %.2f\n', queue(front).size, sojourn_time);
            front = front + 1;
            length = length - 1;
            dropped = dropped + 1;
            drop_count = drop_count + 1;
            drop_next = current_time + interval / sqrt(drop_count);
        else
            fprintf('Packet dequeued, size: %d, sojourn time: %.2f\n', queue(front).size, sojourn_time);
            front = front + 1;
            length = length - 1;
            drop_count = 0;
        end
        current_time = current_time + 1;
    end

    fprintf('Final queue length: %d\n', length);
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: empty\n');
end
