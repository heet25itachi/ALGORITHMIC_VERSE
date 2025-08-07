function external_bfs
    function arr = remove_duplicates(arr)
        arr = sort(arr);
        arr = unique(arr);
    end

    function c = set_difference(a, b)
        c = setdiff(a, b);
    end

    n = 10;
    adj = cell(n, 1);
    edges = [0 1; 0 3; 0 9; 1 0; 1 2; 1 4; 2 1; 2 3; ...
             3 0; 3 2; 3 4; 4 1; 4 3; 4 5; 5 4; 5 6; ...
             5 8; 6 5; 6 7; 7 6; 7 8; 8 5; 8 7; 8 9; ...
             9 0; 9 8] + 1; % MATLAB 1-based indexing
    for i = 1:size(edges, 1)
        adj{edges(i, 1)} = [adj{edges(i, 1)}, edges(i, 2)];
    end

    disp('Adjacency lists:');
    for i = 1:n
        fprintf('%d: %s\n', i-1, num2str(adj{i}-1));
    end

    M = 5;
    B = 2;
    visited = false(1, n);
    level = zeros(1, M);
    prev_level = zeros(1, M);
    prev_prev_level = zeros(1, M);
    level_size = 0;
    prev_level_size = 0;
    prev_prev_level_size = 0;
    t = 0;

    % Initialize L(0) = {start}
    start = 1; % 1-based
    level(1) = start;
    level_size = 1;
    visited(start) = true;
    fprintf('Level %d: %d\n', t, start-1);

    while level_size > 0
        % Step 1: Compute A(t) = neighbors of L(t-1)
        neighbors = [];
        for i = 1:level_size
            v = level(i);
            neighbors = [neighbors, adj{v}];
        end

        % Step 2: Compute A'(t) by removing duplicates
        neighbors = remove_duplicates(neighbors);

        % Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
        temp = [prev_level(1:prev_level_size), prev_prev_level(1:prev_prev_level_size)];
        temp = remove_duplicates(temp);
        new_level = set_difference(neighbors, temp);

        % Update visited
        for v = new_level
            visited(v) = true;
        end

        % Print current level
        if ~isempty(new_level)
            fprintf('Level %d: %s\n', t+1, num2str(new_level-1));
        end

        % Update levels
        prev_prev_level_size = prev_level_size;
        prev_prev_level = prev_level;
        prev_level_size = level_size;
        prev_level = level;
        level_size = length(new_level);
        level(1:level_size) = new_level;
        t = t + 1;
    end
end
