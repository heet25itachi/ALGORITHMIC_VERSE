function external_merge_sort
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

    function output = merge_runs(run1, run2, B)
        output = [];
        i = 1;
        j = 1;
        while i <= length(run1) && j <= length(run2)
            for b = 1:B
                if i <= length(run1) && (j > length(run2) || run1(i) <= run2(j))
                    output(end + 1) = run1(i);
                    i = i + 1;
                elseif j <= length(run2)
                    output(end + 1) = run2(j);
                    j = j + 1;
                end
            end
        end
        output = [output, run1(i:end), run2(j:end)];
    end

    arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13];
    M = 8;
    B = 4;
    n = length(arr);
    disp(['Initial array: ', num2str(arr)]);
    if n <= M
        quicksort(arr, 1, n);
        disp(['Sorted array: ', num2str(arr)]);
        return;
    end

    % Step 1: Divide and sort runs
    runs = {};
    for i = 1:M:n
        run = arr(i:min(i+M-1, n));
        quicksort(run, 1, length(run));
        runs{end + 1} = run;
    end

    % Print sorted runs
    disp('Sorted runs:');
    for i = 1:length(runs)
        disp(['Run ', num2str(i-1), ': ', num2str(runs{i})]);
    end

    % Step 2: Merge runs (2-way merge for M/B = 2)
    output = merge_runs(runs{1}, runs{2}, B);
    arr = output;
    disp(['Sorted array: ', num2str(arr)]);
end
