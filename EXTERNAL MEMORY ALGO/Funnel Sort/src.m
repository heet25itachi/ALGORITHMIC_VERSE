function funnelsort(arr, z, l)
    function quicksort(arr, left, right)
        if left >= right
            return
        end
        pivot = arr(right + 1);
        i = left + 1;
        j = right + 1;
        while i < j
            while i < j && arr(i) <= pivot
                i = i + 1;
            end
            while i < j && arr(j) > pivot
                j = j - 1;
            end
            if i < j
                arr([i j]) = arr([j i]);
            end
        end
        arr([right + 1 i]) = arr([i right + 1]);
        quicksort(arr, left, i - 1);
        quicksort(arr, i + 1, right);
    end

    function output = k_merger(inputs, input_sizes, k, buffer, buffer_size, k3)
        if k == 1
            size = min(input_sizes(1), k3);
            output = inputs{1}(1:size);
            inputs{1} = inputs{1}(size + 1:end);
            input_sizes(1) = input_sizes(1) - size;
            return
        end

        sqrt_k = floor(sqrt(k));
        input_mergers = cell(sqrt_k, 1);
        input_merger_sizes = zeros(sqrt_k, 1);
        sub_buffers = cell(sqrt_k, 1);
        sub_buffer_sizes = zeros(sqrt_k, 1);
        for i = 1:sqrt_k
            input_mergers{i} = inputs{i * sqrt_k};
            input_merger_sizes(i) = input_sizes(i * sqrt_k);
            sub_buffers{i} = zeros(1, buffer_size);
        end

        k32 = floor(k^1.5);
        for i = 1:sqrt_k
            if sub_buffer_sizes(i) < k32
                temp_output = k_merger(input_mergers, input_merger_sizes, sqrt_k, sub_buffers{i}, buffer_size, k32);
                sub_buffers{i}(1:length(temp_output)) = temp_output;
                sub_buffer_sizes(i) = length(temp_output);
            end
        end

        output_merger_inputs = sub_buffers;
        output_merger_sizes = sub_buffer_sizes;
        output = k_merger(output_merger_inputs, output_merger_sizes, sqrt_k, buffer, buffer_size, k3);

        for i = 1:sqrt_k
            sub_buffers{i} = sub_buffers{i}(length(output) + 1:end);
            sub_buffer_sizes(i) = sub_buffer_sizes(i) - length(output);
        end
    end

    n = length(arr);
    if n <= z
        quicksort(arr, 1, n);
        return
    end

    k = ceil(n^(1/3));
    sub_size = ceil(n / k);
    subarrays = cell(k, 1);
    subarray_sizes = zeros(k, 1);

    disp('Sorted subarrays:');
    for i = 1:k
        size = n - (i-1) * sub_size;
        if i < k
            size = sub_size;
        end
        subarrays{i} = arr((i-1) * sub_size + 1:(i-1) * sub_size + size);
        subarray_sizes(i) = size;
        quicksort(subarrays{i}, 1, size);
        fprintf('Subarray %d: %s\n', i-1, num2str(subarrays{i}));
    end

    buffer_size = 2 * floor(k^1.5);
    buffer = zeros(1, buffer_size);
    output = k_merger(subarrays, subarray_sizes, k, buffer, buffer_size, n);

    arr(1:n) = output;
end

% Main script
arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13];
z = 8;
l = 2;

fprintf('Initial array: %s\n', num2str(arr));

funnelsort(arr, z, l);

fprintf('Sorted array: %s\n', num2str(arr));
