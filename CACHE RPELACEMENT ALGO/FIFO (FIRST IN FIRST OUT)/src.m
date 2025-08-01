function fifo_cache()
    capacity = 3;
    cache = containers.Map('KeyType', 'int32', 'ValueType', 'int32');
    order = [];

    function value = get(key)
        if isKey(cache, key)
            value = cache(key);
            fprintf('Cache after get(%d): [', key);
            for i = 1:length(order)
                fprintf('(%d, %d)', order(i), cache(order(i)));
                if i < length(order)
                    fprintf(', ');
                end
            end
            fprintf(']\n');
        else
            value = -1;
        end
    end

    function put(key, value)
        if isKey(cache, key)
            cache(key) = value;
        else
            if length(cache) == capacity
                remove(cache, order(1));
                order(1) = [];
            end
            cache(key) = value;
            order = [order, key];
        end
        fprintf('Cache after put(%d, %d): [', key, value);
        for i = 1:length(order)
            fprintf('(%d, %d)', order(i), cache(order(i)));
            if i < length(order)
                fprintf(', ');
            end
        end
        fprintf(']\n');
    end

    put(1, 10);
    put(2, 20);
    put(3, 30);
    put(4, 40);
    fprintf('Get(2) = %d\n', get(2));
    put(5, 50);
end
