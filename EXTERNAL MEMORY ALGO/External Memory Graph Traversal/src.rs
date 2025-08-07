use std::collections::HashSet;

fn remove_duplicates(arr: &mut Vec<i32>) {
    arr.sort();
    arr.dedup();
}

fn set_difference(a: &[i32], b: &[i32]) -> Vec<i32> {
    let b_set: HashSet<i32> = b.iter().cloned().collect();
    a.iter().filter(|&x| !b_set.contains(x)).cloned().collect()
}

fn external_bfs(adj: &[Vec<i32>], start: i32, m: usize, _b: usize) {
    let n = adj.len();
    let mut visited = vec![false; n];
    let mut level = vec![0; m];
    let mut prev_level = vec![0; m];
    let mut prev_prev_level = vec![0; m];
    let mut level_size = 0;
    let mut prev_level_size = 0;
    let mut prev_prev_level_size = 0;
    let mut t = 0;

    // Initialize L(0) = {start}
    level[0] = start;
    level_size = 1;
    visited[start as usize] = true;
    println!("Level {}: {}", t, start);

    while level_size > 0 {
        // Step 1: Compute A(t) = neighbors of L(t-1)
        let mut neighbors = vec![];
        for i in 0..level_size {
            let v = level[i];
            neighbors.extend_from_slice(&adj[v as usize]);
        }

        // Step 2: Compute A'(t) by removing duplicates
        remove_duplicates(&mut neighbors);

        // Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
        let mut temp = vec![];
        temp.extend_from_slice(&prev_level[..prev_level_size]);
        temp.extend_from_slice(&prev_prev_level[..prev_prev_level_size]);
        remove_duplicates(&mut temp);
        let new_level = set_difference(&neighbors, &temp);

        // Update visited
        for &v in &new_level {
            visited[v as usize] = true;
        }

        // Print current level
        if !new_level.is_empty() {
            print!("Level {}: ", t + 1);
            for v in &new_level {
                print!("{} ", v);
            }
            println!();
        }

        // Update levels
        prev_prev_level_size = prev_level_size;
        prev_prev_level.copy_from_slice(&prev_level[..prev_level_size]);
        prev_level_size = level_size;
        prev_level.copy_from_slice(&level[..level_size]);
        level_size = new_level.len();
        level[..new_level.len()].copy_from_slice(&new_level);
        t += 1;
    }
}

fn main() {
    let n = 10;
    let mut adj = vec![vec![]; n];
    let edges = [
        (0,1), (0,3), (0,9), (1,0), (1,2), (1,4), (2,1), (2,3),
        (3,0), (3,2), (3,4), (4,1), (4,3), (4,5), (5,4), (5,6),
        (5,8), (6,5), (6,7), (7,6), (7,8), (8,5), (8,7), (8,9),
        (9,0), (9,8)
    ];
    for &(u, v) in &edges {
        adj[u].push(v as i32);
    }

    println!("Adjacency lists:");
    for i in 0..n {
        print!("{}: ", i);
        for &v in &adj[i] {
            print!("{} ", v);
        }
        println!();
    }

    external_bfs(&adj, 0, 5, 2);
}
