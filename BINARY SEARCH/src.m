function index = binary_search(arr, target)
    left = 1;
    right = length(arr);
    while left <= right
        mid = floor((left + right) / 2);
        if arr(mid) == target
            index = mid;
            return;
        elseif arr(mid) < target
            left = mid + 1;
        else
            right = mid - 1;
        end
    end
    index = -1;
end

arr = [1, 3, 4, 7, 9];
target = 9;
result = binary_search(arr, target);
fprintf('Target %d found at index: %d\n', target, result);
