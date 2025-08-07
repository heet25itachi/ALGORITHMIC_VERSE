function remove_duplicates(arr::Vector{Int})
    sort!(arr)
    unique(arr)
end

function set_difference(a::Vector{Int}, b::Vector{Int})
    setdiff(a, b)
end

function external_bfs(adj::Vector{Vector{Int}}, start::Int, M::Int, B::Int)
    n = length(adj)
    visited = falses(n)
    level = zeros(Int, M)
    prev_level = zeros(Int, M)
    prev_prev_level = zeros(Int, M)
    level_size = prev_level_size = prev_prev_level_size = 0
    t = 0

    # Initialize L(0) = {start}
    level[1] = start
    level_size = 1
    visited[start + 1] = true  # 1-based indexing
    println("Level $t: $start")

    while level_size > 0
        # Step 1: Compute A(t) = neighbors of L(t-1)
        neighbors = Int[]
        for i in 1:level_size
            v = level[i]
            append!(neighbors, adj[v + 1])
        end

        # Step 2: Compute A'(t) by removing duplicates
        neighbors = remove_duplicates(neighbors)

        # Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
        temp = [prev_level[1:prev_level_size]; prev_prev_level[1:prev_prev_level_size]]
        temp = remove_duplicates(temp)
        new_level = set_difference(neighbors, temp)

        # Update visited
        for v in new_level
            visited[v + 1] = true
        end

        # Print current level
        if !isempty(new_level)
            println("Level $(t+1): ", join(new_level .- 1, " "))
        end

        # Update levels
        prev_prev_level_size = prev_level_size
        prev_prev_level = copy(prev_level)
        prev_level_size = level_size
        prev_level = copy(level)
        level_size = length(new_level)
        level[1:level_size] = new_level
        t += 1
    end
end

n = 10
adj = [Int[] for _ in 1:n]
edges = [(0,1), (0,3), (0,9), (1,0), (1,2), (1,4), (2,1), (2,3),
         (3,0), (3,2), (3,4), (4,1), (4,3), (4,5), (5,4), (5,6),
         (5,8), (6,5), (6,7), (7,6), (7,8), (8,5), (8,7), (8,9),
         (9,0), (9,8)]
for (u, v) in edges
    push!(adj[u + 1], v + 1)  # 1-based indexing
end

println("Adjacency lists:")
for i in 1:n
    println("$(i-1): ", join(adj[i] .- 1, " "))
end

external_bfs(adj, 1, 5, 2)  # Start from vertex 0 (1-based)
