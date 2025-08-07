function cache_oblivious_sort
    function median = approximate_median(arr)
        if isempty(arr)
            median = 0;
            return;
        end
        mid = floor(length(arr) / 2) + 1;
        values = [arr(1), arr(mid), arr(end)];
        median = median(values);
    end

    function quicksort(arr, left, right)
        if left >= right
            return;
        end
        pivot = arr(right);
        i = left;
        j = right;
        while i < j
            while i < j && arr(i) <= pivot
                i = i + 1;
            end
            while i < j && arr(j) > pivot
                j = j - 1;
            end
            if i < j
                arr([i, j]) = arr([j, i]);
            end
        end
        arr([i, right]) = arr([right, i]);
        quicksort(arr, left, i - 1);
        quicksort(arr, i + 1, right);
    end

    function [arr, next, bnum, buckets, num_buckets] = copy_elems(arr, next, bnum, buckets, num_buckets, subarray_size, bucket_idx, sqrt_n)
        while next(bucket_idx + 1) <= subarray_size
            if bnum(bucket_idx + 1) + 1 > num_buckets
                buckets{num_buckets + 1} = struct('elements', [], 'pivot', 1e9);
                num_buckets = num_buckets + 1;
            end
            if arr(next(bucket_idx + 1)) <= buckets{bnum(bucket_idx + 1) + 1}.pivot
                if length(buckets{bnum(bucket_idx + 1) + 1}.elements) >= 2 * sqrt_n
                    median = approximate_median(buckets{bnum(bucket_idx + 1) + 1}.elements);
                    buckets{num_buckets + 1} = struct('elements', [], 'pivot', buckets{bnum(bucket_idx + 1) + 1}.pivot);
                    buckets{bnum(bucket_idx + 1) + 1}.pivot = median;
                    new_elements = buckets{bnum(bucket_idx + 1) + 1}.elements(buckets{bnum(bucket_idx + 1) + 1}.elements <= median);
                    buckets{num_buckets + 1}.elements = buckets{bnum(bucket_idx + 1) + 1}.elements(buckets{bnum(bucket_idx + 1) + 1}.elements > median);
                    buckets{bnum(bucket_idx + 1) + 1}.elements = new_elements;
                    num_buckets = num_buckets + 1;
                    bnum(bnum > bnum(bucket_idx + 1)) = bnum(bnum > bnum(bucket_idx + 1)) + 1;
                end
                buckets{bnum(bucket_idx + 1) + 1}.elements(end + 1) = arr(next(bucket_idx + 1));
                next(bucket_idx + 1) = next(bucket_idx + 1) + 1;
            else
                bnum(bucket_idx + 1) = bnum(bucket_idx + 1) + 1;
            end
        end
    end

    function [arr, next, bnum, buckets, num_buckets] = distribute(arr, next, bnum, i, j, m, buckets, num_buckets, sqrt_n)
        if m == 1
            [arr((i-1)*sqrt_n + 1:i*sqrt_n), next, bnum, buckets, num_buckets] = copy_elems(arr((i-1)*sqrt_n + 1:i*sqrt_n), next, bnum, buckets, num_buckets, sqrt_n, i, sqrt_n);
        else
            [arr, next, bnum, buckets, num_buckets] = distribute(arr, next, bnum, i, j, m/2, buckets, num_buckets, sqrt_n);
            [arr, next, bnum, buckets, num_buckets] = distribute(arr, next, bnum, i + m/2, j, m/2, buckets, num_buckets, sqrt_n);
            [arr, next, bnum, buckets, num_buckets] = distribute(arr, next, bnum, i, j + m/2, m/2, buckets, num_buckets, sqrt_n);
            [arr, next, bnum, buckets, num_buckets] = distribute(arr, next, bnum, i + m/2, j + m/2, m/2, buckets, num_buckets, sqrt_n);
        end
    end

    arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13];
    n = length(arr);
    disp(['Initial array: ', num2str(arr)]);
    if n <= 1
        return;
    end
    sqrt_n = floor(sqrt(n));
    if sqrt_n * sqrt_n ~= n
        return;
    end

    % Step 1: Partition and sort subarrays
    for i = 1:sqrt_n
        quicksort(arr, (i-1)*sqrt_n + 1, i*sqrt_n);
    end

    % Step 2: Distribute
    next = zeros(1, sqrt_n);
    bnum = zeros(1, sqrt_n);
    buckets = {struct('elements', [], 'pivot', 1e9)};
    num_buckets = 1;
    [arr, next, bnum, buckets, num_buckets] = distribute(arr, next, bnum, 1, 1, sqrt_n, buckets, num_buckets, sqrt_n);

    % Step 3: Sort buckets
    for i = 1:num_buckets
        buckets{i}.elements = sort(buckets{i}.elements);
    end

    % Step 4: Concatenate
    k = 1;
    for i = 1:num_buckets
        for x = buckets{i}.elements
            arr(k) = x;
            k = k + 1;
        end
    end
    disp(['Sorted array: ', num2str(arr)]);
end
