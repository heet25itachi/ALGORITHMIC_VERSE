import java.util.*;

public class ExternalBFS {
    static void removeDuplicates(int[] arr, int[] size) {
        Arrays.sort(arr, 0, size[0]);
        int k = 1;
        for (int i = 1; i < size[0]; i++) {
            if (arr[i] != arr[k - 1]) arr[k++] = arr[i];
        }
        size[0] = k;
    }

    static void setDifference(int[] a, int na, int[] b, int nb, int[] c, int[] nc) {
        int i = 0, j = 0, k = 0;
        while (i < na) {
            if (j < nb && a[i] == b[j]) {
                i++; j++;
            } else {
                c[k++] = a[i++];
            }
        }
        nc[0] = k;
    }

    static void externalBFS(List<List<Integer>> adj, int start, int M, int B) {
        int n = adj.size();
        boolean[] visited = new boolean[n];
        int[] level = new int[M];
        int[] prevLevel = new int[M];
        int[] prevPrevLevel = new int[M];
        int[] neighbors = new int[n];
        int levelSize = 0, prevLevelSize = 0, prevPrevLevelSize = 0;
        int t = 0;

        // Initialize L(0) = {start}
        level[0] = start;
        levelSize = 1;
        visited[start] = true;
        System.out.println("Level " + t + ": " + start);

        while (levelSize > 0) {
            // Step 1: Compute A(t) = neighbors of L(t-1)
            int neighborSize = 0;
            for (int i = 0; i < levelSize; i++) {
                int v = level[i];
                for (int u : adj.get(v)) {
                    neighbors[neighborSize++] = u;
                }
            }

            // Step 2: Compute A'(t) by removing duplicates
            int[] size = {neighborSize};
            removeDuplicates(neighbors, size);
            neighborSize = size[0];

            // Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
            int[] temp = new int[n];
            int tempSize = 0;
            for (int i = 0; i < prevLevelSize; i++) temp[tempSize++] = prevLevel[i];
            for (int i = 0; i < prevPrevLevelSize; i++) temp[tempSize++] = prevPrevLevel[i];
            size[0] = tempSize;
            removeDuplicates(temp, size);
            tempSize = size[0];

            int[] newLevel = new int[n];
            int[] newLevelSize = {0};
            setDifference(neighbors, neighborSize, temp, tempSize, newLevel, newLevelSize);

            // Update visited
            for (int i = 0; i < newLevelSize[0]; i++) visited[newLevel[i]] = true;

            // Print current level
            if (newLevelSize[0] > 0) {
                System.out.print("Level " + (t + 1) + ": ");
                for (int i = 0; i < newLevelSize[0]; i++) System.out.print(newLevel[i] + " ");
                System.out.println();
            }

            // Update levels
            prevPrevLevelSize = prevLevelSize;
            System.arraycopy(prevLevel, 0, prevPrevLevel, 0, prevLevelSize);
            prevLevelSize = levelSize;
            System.arraycopy(level, 0, prevLevel, 0, levelSize);
            levelSize = newLevelSize[0];
            System.arraycopy(newLevel, 0, level, 0, newLevelSize[0]);
            t++;
        }
    }

    public static void main(String[] args) {
        int n = 10;
        List<List<Integer>> adj = new ArrayList<>();
        for (int i = 0; i < n; i++) adj.add(new ArrayList<>());
        int[][] edges = {{0,1}, {0,3}, {0,9}, {1,0}, {1,2}, {1,4}, {2,1}, {2,3},
                         {3,0}, {3,2}, {3,4}, {4,1}, {4,3}, {4,5}, {5,4}, {5,6},
                         {5,8}, {6,5}, {6,7}, {7,6}, {7,8}, {8,5}, {8,7}, {8,9},
                         {9,0}, {9,8}};
        for (int[] e : edges) adj.get(e[0]).add(e[1]);

        System.out.println("Adjacency lists:");
        for (int i = 0; i < n; i++) {
            System.out.print(i + ": ");
            for (int v : adj.get(i)) System.out.print(v + " ");
            System.out.println();
        }

        externalBFS(adj, 0, 5, 2);
    }
}
