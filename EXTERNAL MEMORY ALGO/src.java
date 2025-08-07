import java.util.*;

public class ExternalMergeSort {
    static void quicksort(int[] arr, int left, int right) {
        if (left >= right) return;
        int pivot = arr[right], i = left, j = right;
        while (i < j) {
            while (i < j && arr[i] <= pivot) i++;
            while (i < j && arr[j] > pivot) j--;
            if (i < j) {
                int tmp = arr[i]; arr[i] = arr[j]; arr[j] = tmp;
            }
        }
        int tmp = arr[i]; arr[i] = arr[right]; arr[right] = tmp;
        quicksort(arr, left, i - 1);
        quicksort(arr, i + 1, right);
    }

    static void mergeRuns(int[] run1, int[] run2, int[] output, int B) {
        int i = 0, j = 0, k = 0;
        while (i < run1.length && j < run2.length) {
            for (int b = 0; b < B && i < run1.length && j < run2.length; b++) {
                if (run1[i] <= run2[j]) output[k++] = run1[i++];
                else output[k++] = run2[j++];
            }
        }
        while (i < run1.length) output[k++] = run1[i++];
        while (j < run2.length) output[k++] = run2[j++];
    }

    static void externalMergeSort(int[] arr, int M, int B) {
        int n = arr.length;
        if (n <= M) {
            quicksort(arr, 0, n - 1);
            return;
        }

        // Step 1: Divide and sort runs
        List<int[]> runs = new ArrayList<>();
        for (int i = 0; i < n; i += M) {
            int size = Math.min(M, n - i);
            int[] run = Arrays.copyOfRange(arr, i, i + size);
            quicksort(run, 0, size - 1);
            runs.add(run);
        }

        // Print sorted runs
        System.out.println("Sorted runs:");
        for (int i = 0; i < runs.size(); i++) {
            System.out.print("Run " + i + ": ");
            for (int x : runs.get(i)) System.out.print(x + " ");
            System.out.println();
        }

        // Step 2: Merge runs (2-way merge for M/B = 2)
        int[] output = new int[n];
        mergeRuns(runs.get(0), runs.get(1), output, B);
        for (int i = 0; i < n; i++) arr[i] = output[i];
    }

    public static void main(String[] args) {
        int[] arr = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
        int M = 8, B = 4;
        System.out.print("Initial array: ");
        for (int x : arr) System.out.print(x + " ");
        System.out.println();
        externalMergeSort(arr, M, B);
        System.out.print("Sorted array: ");
        for (int x : arr) System.out.print(x + " ");
        System.out.println();
    }
}
