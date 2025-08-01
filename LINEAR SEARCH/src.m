function index = linear_search(arr, target)
    index = -1;
    for i = 1:length(arr)
        if arr(i) == target
            index = i;
            return;
        end
    end
end

arr = [3, 7, 1, 9, 4];
target = 9;
result = linear_search(arr, target);
fprintf('Target %d found at index: %d\n', target, result);
