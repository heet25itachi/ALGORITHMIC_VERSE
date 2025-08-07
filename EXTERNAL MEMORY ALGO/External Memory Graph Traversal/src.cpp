#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

void remove_duplicates(vector<int>& arr) {
    sort(arr.begin(), arr.end());
    arr.erase(unique(arr.begin(), arr.end()), arr.end());
}

void set_difference(vector<int>& a, vector<int>& b, vector<int>& c) {
    c.clear();
    set_difference(a.begin(), a.end(), b.begin(), b.end(), back_inserter(c));
}

void external_bfs(vector<vector<int>>& adj, int start, int M, int B) {
    int n = adj.size();
    vector<bool> visited(n, false);
    vector<int> level(M), prev_level(M), prev_prev_level(M), neighbors(n);
    int level_size = 0, prev_level_size = 0, prev_prev_level_size = 0;
    int t = 0;

    // Initialize L(0) = {start}
    level[0] = start;
    level_size = 1;
    visited[start] = true;
    cout << "Level " << t << ": " << start << endl;

    while (level_size > 0) {
        // Step 1: Compute A(t) = neighbors of L(t-1)
        neighbors.clear();
        for (int i = 0; i < level_size; i++) {
            int v = level[i];
            neighbors.insert(neighbors.end(), adj[v].begin(), adj[v].end());
        }

        // Step 2: Compute A'(t) by removing duplicates
        remove_duplicates(neighbors);

        // Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
        vector<int> temp;
        temp.insert(temp.end(), prev_level.begin(), prev_level.begin() + prev_level_size);
        temp.insert(temp.end(), prev_prev_level.begin(), prev_prev_level.begin() + prev_prev_level_size);
        remove_duplicates(temp);

        vector<int> new_level;
        set_difference(neighbors, temp, new_level);

        // Update visited
        for (int v : new_level) visited[v] = true;

        // Print current level
        if (!new_level.empty()) {
            cout << "Level " << t + 1 << ": ";
            for (int v : new_level) cout << v << " ";
            cout << endl;
        }

        // Update levels
        prev_prev_level = prev_level;
        prev_prev_level_size = prev_level_size;
        prev_level = level;
        prev_level_size = level_size;
        level = new_level;
        level_size = new_level.size();
        t++;
    }
}

int main() {
    int n = 10;
    vector<vector<int>> adj(n);
    int edges[][2] = {{0,1}, {0,3}, {0,9}, {1,0}, {1,2}, {1,4}, {2,1}, {2,3},
                      {3,0}, {3,2}, {3,4}, {4,1}, {4,3}, {4,5}, {5,4}, {5,6},
                      {5,8}, {6,5}, {6,7}, {7,6}, {7,8}, {8,5}, {8,7}, {8,9},
                      {9,0}, {9,8}};
    for (int i = 0; i < 26; i++) adj[edges[i][0]].push_back(edges[i][1]);

    cout << "Adjacency lists:\n";
    for (int i = 0; i < n; i++) {
        cout << i << ": ";
        for (int v : adj[i]) cout << v << " ";
        cout << endl;
    }

    external_bfs(adj, 0, 5, 2);
    return 0;
}
