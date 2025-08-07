def remove_duplicates(arr)
  arr.sort.uniq
end

def set_difference(a, b)
  a - b
end

def external_bfs(adj, start, m, b)
  n = adj.size
  visited = Array.new(n, false)
  level = Array.new(m, 0)
  prev_level = Array.new(m, 0)
  prev_prev_level = Array.new(m, 0)
  level_size = prev_level_size = prev_prev_level_size = 0
  t = 0

  # Initialize L(0) = {start}
  level[0] = start
  level_size = 1
  visited[start] = true
  puts "Level #{t}: #{start}"

  while level_size > 0
    # Step 1: Compute A(t) = neighbors of L(t-1)
    neighbors = []
    level[0...level_size].each { |v| neighbors.concat(adj[v]) }

    # Step 2: Compute A'(t) by removing duplicates
    neighbors = remove_duplicates(neighbors)

    # Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
    temp = prev_level[0...prev_level_size] + prev_prev_level[0...prev_prev_level_size]
    temp = remove_duplicates(temp)
    new_level = set_difference(neighbors, temp)

    # Update visited
    new_level.each { |v| visited[v] = true }

    # Print current level
    puts "Level #{t + 1}: #{new_level.join(' ')}" unless new_level.empty?

    # Update levels
    prev_prev_level_size = prev_level_size
    prev_prev_level = prev_level.dup
    prev_level_size = level_size
    prev_level = level.dup
    level_size = new_level.size
    level[0...level_size] = new_level
    t += 1
  end
end

n = 10
adj = Array.new(n) { [] }
edges = [[0,1], [0,3], [0,9], [1,0], [1,2], [1,4], [2,1], [2,3],
         [3,0], [3,2], [3,4], [4,1], [4,3], [4,5], [5,4], [5,6],
         [5,8], [6,5], [6,7], [7,6], [7,8], [8,5], [8,7], [8,9],
         [9,0], [9,8]]
edges.each { |u, v| adj[u] << v }

puts "Adjacency lists:"
adj.each_with_index { |list, i| puts "#{i}: #{list.join(' ')}" }

external_bfs(adj, 0, 5, 2)
