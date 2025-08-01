function index = jump_search(arr, target)
    size = length(arr);
    step = floor(sqrt(size));
    prev = 1;
    while arr(min(step, size)) < target
        prev = step;
        step = step + floor(sqrt(size));
        if prev > size
            index = -1;
            return;
        end
    end
    while prev <= size && arr(prev) < target
        prev = prev + 1;
    end
    if prev <= size && arr(prev) == target
        index = prev;
    else
        index = -1;
    end
end

arr = [1, 3, 4, 7, 9];
target = 9;
result = jump_search(arr, target);
fprintf('Target %d found at index: %d\n', target, result);
