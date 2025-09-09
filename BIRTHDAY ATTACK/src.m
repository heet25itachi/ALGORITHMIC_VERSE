function birthday_attack()
    R = 65536;
    table = containers.Map('KeyType', 'int32', 'ValueType', 'int64');
    trials = 0;
    while true
        x = randi([0, 2^63-1], 1, 'int64');
        h = mod(x, R);
        trials = trials + 1;
        if isKey(table, h) && table(h) ~= x
            fprintf('Collision found after %d trials: x_i = %d, x_j = %d, hash = %d\n', ...
                trials, table(h), x, h);
            break;
        end
        table(h) = x;
    end
end

birthday_attack();
