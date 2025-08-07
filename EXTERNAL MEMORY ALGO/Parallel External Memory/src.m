function pem_dist_sort(arr, p, m, b, d)
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

    function pivot = pem_select(arr, n, k)
        if n <= 5
            temp = sort(arr(1:n));
            pivot = temp(k);
            return
        end
        pivot = arr(k); % Simplified: use direct access for small arrays
    end

    function [buckets, bucket_sizes] = pem_multipartition(arr, n, pivots, d_sqrt, p)
        counts = zeros(1, p * d_sqrt);
        for i = 1:p
            start = (i-1) * (n/p) + 1;
            size = n - (i-1) * (n/p);
            if i < p
                size = n/p;
            end
            for j = 1:size
                elem = arr(start + j - 1);
                bucket = 1;
                while bucket < d_sqrt && elem > pivots(bucket)
                    bucket = bucket + 1;
                end
                counts((i-1) * d_sqrt + bucket) = counts((i-1) * d_sqrt + bucket) + 1;
            end
        end

        prefix_sums = zeros(1, d_sqrt);
        for j = 1:d_sqrt
            for i = 1:p
                prefix_sums(j) = prefix_sums(j) + counts((i-1) * d_sqrt + j);
            end
        end

        buckets = cell(1, d_sqrt);
        bucket_sizes = prefix_sums;
        for j = 1:d_sqrt
            buckets{j} = zeros(1, prefix_sums(j));
        end

        offsets = zeros(1, d_sqrt);
        for i = 1:p
            start = (i-1) * (n/p) + 1;
            size = n - (i-1) * (n/p);
            if i < p
                size = n/p;
            end
            for j = 1:size
                elem = arr(start + j - 1);
                bucket = 1;
                while bucket < d_sqrt && elem > pivots(bucket)
                    bucket = bucket + 1;
                end
                offsets(bucket) = offsets(bucket) + 1;
                buckets{bucket}(offsets(bucket)) = elem;
            end
        end
    end

    n = length(arr);
    if n <= m
        quicksort(arr, 1, n);
        return
    end

    d_sqrt = floor(sqrt(d));
    segment_size = n / p;

    disp('Segments:');
    for i = 1:p
        size = n - (i-1) * segment_size;
        if i < p
            size = segment_size;
        end
        fprintf('Segment %d: %s\n', i-1, num2str(arr((i-1)*segment_size + 1:(i-1)*segment_size + size)));
    end

    pivots = zeros(1, d_sqrt - 1);
    for j = 1:d_sqrt-1
        pivots(j) = pem_select(arr, n, (j * n) / d_sqrt);
    end
    fprintf('Pivots: %s\n', num2str(pivots));

    [buckets, bucket_sizes] = pem_multipartition(arr, n, pivots, d_sqrt, p);

    disp('Buckets:');
    for j = 1:d_sqrt
        fprintf('Bucket %d: %s\n', j-1, num2str(buckets{j}));
    end

    output = zeros(1, n);
    output_pos = 1;
    for j = 1:d_sqrt
        processors = ceil(bucket_sizes(j) / (n/p));
        pem_dist_sort(buckets{j}, processors, m, b, d);
        output(output_pos:output_pos + bucket_sizes(j) - 1) = buckets{j};
        output_pos = output_pos + bucket_sizes(j);
    end

    arr(1:n) = output;
end

% Main script
arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13];
p = 4; m = 8; b = 2; d = 4;

fprintf('Initial array: %s\n', num2str(arr));

pem_dist_sort(arr, p, m, b, d);

fprintf('Sorted array: %s\n', num2str(arr));
