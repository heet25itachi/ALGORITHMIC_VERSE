#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void sort(int *arr, int n) {
    for (int i = 0; i < n - 1; i++)
        for (int j = 0; j < n - i - 1; j++)
            if (arr[j] > arr[j + 1]) {
                int tmp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = tmp;
            }
}

void remove_duplicates(int *arr, int *n) {
    if (*n <= 1) return;
    sort(arr, *n);
    int k = 1;
    for (int i = 1; i < *n; i++)
        if (arr[i] != arr[k - 1])
            arr[k++] = arr[i];
    *n = k;
}

void set_difference(int *a, int na, int *b, int nb, int *c, int *nc) {
    int i = 0, j = 0, k = 0;
    while (i < na) {
        if (j < nb && a[i] == b[j]) {
            i++; j++;
        } else {
            c[k++] = a[i++];
        }
    }
    *nc = k;
}

void external_bfs(int **adj, int *adj_sizes, int n, int start, int M, int B) {
    int *visited = (int *)calloc(n, sizeof(int));
    int *level = (int *)malloc(M * sizeof(int));
    int *prev_level = (int *)malloc(M * sizeof(int));
    int *prev_prev_level = (int *)malloc(M * sizeof(int));
    int *neighbors = (int *)malloc(n * sizeof(int));
    int level_size = 0, prev_level_size = 0, prev_prev_level_size = 0;
    int t = 0;

    // Initialize L(0) = {start}
    level[0] = start;
    level_size = 1;
    visited[start] = 1;
    printf("Level %d: %d\n", t, start);

    while (level_size > 0) {
        // Step 1: Compute A(t) = neighbors of L(t-1)
        int neighbor_size = 0;
        for (int i = 0; i < level_size; i++) {
            int v = level[i];
            for (int j = 0; j < adj_sizes[v]; j++) {
                neighbors[neighbor_size++] = adj[v][j];
            }
        }

        // Step 2: Compute A'(t) by removing duplicates
        remove_duplicates(neighbors, &neighbor_size);

        // Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
        int *temp = (int *)malloc(n * sizeof(int));
        int temp_size = 0;
        for (int i = 0; i < prev_level_size; i++) temp[temp_size++] = prev_level[i];
        for (int i = 0; i < prev_prev_level_size; i++) temp[temp_size++] = prev_prev_level[i];
        remove_duplicates(temp, &temp_size);

        int *new_level = (int *)malloc(n * sizeof(int));
        int new_level_size = 0;
        set_difference(neighbors, neighbor_size, temp, temp_size, new_level, &new_level_size);

        // Update visited and prepare for next iteration
        for (int i = 0; i < new_level_size; i++) visited[new_level[i]] = 1;

        // Print current level
        if (new_level_size > 0) {
            printf("Level %d: ", t + 1);
            for (int i = 0; i < new_level_size; i++) printf("%d ", new_level[i]);
            printf("\n");
        }

        // Update levels
        prev_prev_level_size = prev_level_size;
        for (int i = 0; i < prev_level_size; i++) prev_prev_level[i] = prev_level[i];
        prev_level_size = level_size;
        for (int i = 0; i < level_size; i++) prev_level[i] = level[i];
        level_size = new_level_size;
        for (int i = 0; i < new_level_size; i++) level[i] = new_level[i];
        t++;

        free(temp);
        free(new_level);
    }

    free(visited);
    free(level);
    free(prev_level);
    free(prev_prev_level);
    free(neighbors);
}

int main() {
    int n = 10;
    int **adj = (int **)malloc(n * sizeof(int *));
    int adj_sizes[] = {3, 3, 2, 3, 3, 3, 2, 2, 3, 2};
    int edges[][2] = {{0,1}, {0,3}, {0,9}, {1,0}, {1,2}, {1,4}, {2,1}, {2,3},
                      {3,0}, {3,2}, {3,4}, {4,1}, {4,3}, {4,5}, {5,4}, {5,6},
                      {5,8}, {6,5}, {6,7}, {7,6}, {7,8}, {8,5}, {8,7}, {8,9},
                      {9,0}, {9,8}};
    int idx[] = {0, 3, 6, 8, 11, 14, 17, 19, 21, 24, 26};
    for (int i = 0; i < n; i++) {
        adj[i] = (int *)malloc(adj_sizes[i] * sizeof(int));
        for (int j = 0; j < adj_sizes[i]; j++) adj[i][j] = edges[idx[i] + j][1];
    }

    printf("Adjacency lists:\n");
    for (int i = 0; i < n; i++) {
        printf("%d: ", i);
        for (int j = 0; j < adj_sizes[i]; j++) printf("%d ", adj[i][j]);
        printf("\n");
    }

    external_bfs(adj, adj_sizes, n, 0, 5, 2);

    for (int i = 0; i < n; i++) free(adj[i]);
    free(adj);
    return 0;
}
