def remove_duplicates(arr):
    arr.sort()
    return list(dict.fromkeys(arr))

def set_difference(a, b):
    return [x for x in a if x not in set(b)]

def external_bfs(adj, start, M, B):
    n = len(adj)
    visited = [False] * n
    level = [0] * M
    prev_level = [0] * M
    prev_prev_level = [0] * M
    neighbors = []
    level_size = prev_level_size = prev_prev_level_size = 0
    t = 0

    # Initialize L(0) = {start}
    level[0] = start
    level_size = 1
    visited[start] = True
    print(f"Level {t}: {start}")

    while level_size > 0:
        # Step 1: Compute A(t) = neighbors of L(t-1)
        neighbors = []
        for i in range(level_size):
            v = level[i]
            neighbors.extend(adj[v])

        # Step 2: Compute A'(t) by removing duplicates
        neighbors = remove_duplicates(neighbors)

        # Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
        temp = prev_level[:prev_level_size] + prev_prev_level[:prev_prev_level_size]
        temp = remove_duplicates(temp)
        new_level = set_difference(neighbors, temp)

        # Update visited
        for v in new_level:
            visited[v] = True

        # Print current level
        if new_level:
            print(f"Level {t+1}: {' '.join(map(str, new_level))}")

        # Update levels
        prev_prev_level_size = prev_level_size
        prev_prev_level = prev_level[:]
        prev_level_size = level_size
        prev_level = level[:]
        level_size = len(new_level)
        level[:level_size] = new_level
        t += 1

if __name__ == "__main__":
    n = 10
    adj = [[] for _ in range(n)]
    edges = [(0,1), (0,3), (0,9), (1,0), (1,2), (1,4), (2,1), (2,3),
             (3,0), (3,2), (3,4), (4,1), (4,3), (4,5), (5,4), (5,6),
             (5,8), (6,5), (6,7), (7,6), (7,8), (8,5), (8,7), (8,9),
             (9,0), (9,8)]
    for u, v in edges:
        adj[u].append(v)

    print("Adjacency lists:")
    for i in range(n):
        print(f"{i}: {' '.join(map(str, adj[i]))}")

    external_bfs(adj, 0, 5, 2)
