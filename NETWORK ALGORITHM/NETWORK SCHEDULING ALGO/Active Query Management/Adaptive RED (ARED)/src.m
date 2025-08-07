function simulate_aqm()
    % Packet structure
    packets = struct('size', num2cell(randi([1, 100], 1, 200)), 'arrival_time', num2cell(zeros(1, 200)));
    n = 200;
    capacity = 100;

    % ARED
    fprintf('=== ARED ===\n');
    min_th = 20; max_th = 80; w_q = 0.002; target = 50; alpha = 0.01; beta = 0.9; interval = 1000;
    queue = struct('size', cell(1, capacity), 'arrival_time', cell(1, capacity));
    front = 1; rear = 0; length = 0;
    avg = 0; max_p = 0.1; last_update = 0; current_time = 0; count = 0; dropped = 0;
    fprintf('Initial queue: empty\n');

    for i = 1:n
        avg = length == 0 ? 0 : (1 - w_q) * avg + w_q * length;
        if current_time - last_update >= interval
            if avg > target && max_p <= 0.5
                max_p = max_p * (1 + alpha);
            elseif avg < target && max_p >= 0.01
                max_p = max_p * beta;
            end
            last_update = current_time;
        end

        drop = false;
        if avg < min_th
            drop = false;
        elseif avg >= max_th
            drop = true;
        else
            pb = max_p * (avg - min_th) / (max_th - min_th);
            pa = pb / (1 - count * pb);
            drop = rand < pa;
            count = count + 1;
        end

        if drop
            fprintf('Packet dropped, size: %d, avg queue length: %.2f, max_p: %.4f\n', packets(i).size, avg, max_p);
            dropped = dropped + 1;
        elseif length < capacity
            rear = rear + 1;
            queue(rear).size = packets(i).size;
            length = length + 1;
            fprintf('Packet enqueued, size: %d, avg queue length: %.2f, max_p: %.4f\n', packets(i).size, avg, max_p);
            count = 0;
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
        current_time = current_time + 1;
    end

    fprintf('Final queue length: %d\n', length);
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: %s\n', num2str([queue(front:front+length-1).size]));

    % Blue
    fprintf('\n=== Blue ===\n');
    queue = struct('size', cell(1, capacity), 'arrival_time', cell(1, capacity));
    front = 1; rear = 0; length = 0;
    p = 0; last_update = 0; current_time = 0; dropped = 0;
    d1 = 0.0002; d2 = 0.00005; freeze_time = 100;
    fprintf('Initial queue: empty\n');

    for i = 1:n
        if length >= capacity
            p = p + d1;
            last_update = current_time;
            fprintf('Queue full, packet dropped, size: %d, drop prob: %.4f\n', packets(i).size, p);
            dropped = dropped + 1;
        else
            if current_time - last_update >= freeze_time && length == 0
                p = max(p - d2, 0);
                last_update = current_time;
            end
            if rand < p
                fprintf('Packet dropped, size: %d, drop prob: %.4f\n', packets(i).size, p);
                dropped = dropped + 1;
            elseif length < capacity
                rear = rear + 1;
                queue(rear).size = packets(i).size;
                length = length + 1;
                fprintf('Packet enqueued, size: %d, drop prob: %.4f\n', packets(i).size, p);
            end
        end
        current_time = current_time + 1;
    end

    fprintf('Final queue length: %d\n', length);
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: %s\n', num2str([queue(front:front+length-1).size]));

    % PI
    fprintf('\n=== PI ===\n');
    queue = struct('size', cell(1, capacity), 'arrival_time', cell(1, capacity));
    front = 1; rear = 0; length = 0;
    p = 0; prev_error = 0; current_time = 0; dropped = 0;
    q_ref = 50; a = 0.00001822; b = 0.00001816;
    fprintf('Initial queue: empty\n');

    for i = 1:n
        error = length - q_ref;
        p = p + a * error - b * prev_error;
        prev_error = error;
        if p < 0
            p = 0;
        elseif p > 1
            p = 1;
        end

        if rand < p
            fprintf('Packet dropped, size: %d, drop prob: %.4f\n', packets(i).size, p);
            dropped = dropped + 1;
        elseif length < capacity
            rear = rear + 1;
            queue(rear).size = packets(i).size;
            length = length + 1;
            fprintf('Packet enqueued, size: %d, drop prob: %.4f\n', packets(i).size, p);
        else
            fprintf('Queue full, packet dropped, size: %d\n', packets(i).size);
            dropped = dropped + 1;
        end
        current_time = current_time + 1;
    end

    fprintf('Final queue length: %d\n', length);
    fprintf('Packets dropped: %d\n', dropped);
    fprintf('Final queue: %s\n', num2str([queue(front:front+length-1).size]));
end
