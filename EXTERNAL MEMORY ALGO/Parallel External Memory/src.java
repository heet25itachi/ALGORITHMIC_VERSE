import java.util.*;

public class PEMDistSort {
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
        arr[right] = arr[i]; arr[i] = pivot;
        quicksort(arr, left, i - 1);
        quicksort(arr, i + 1, right);
    }

    static int pemSelect(int[] arr, int n, int k) {
        if (n <= 5) {
            int[] temp = Arrays.copyOf(arr, n);
            Arrays.sort(temp);
            return temp[k - 1];
        }
        return arr[k - 1]; // Simplified: use sort for small arrays
    }

    static void pemMultipartition(int[] arr, int n, int[] pivots, int dSqrt, int p, List<int[]> buckets, int[] bucketSizes) {
        int[] counts = new int[p * dSqrt];
        for (int i = 0; i < p; i++) {
            int start = i * (n / p);
            int size = i == p - 1 ? n - start : n / p;
            for (int j = 0; j < size; j++) {
                int elem = arr[start + j];
                int bucket = 0;
                while (bucket < dSqrt - 1 && elem > pivots[bucket]) bucket++;
                counts[i * dSqrt + bucket]++;
            }
        }

        int[] prefixSums = new int[dSqrt];
        for (int j = 0; j < dSqrt; j++) {
            for (int i = 0; i < p; i++) prefixSums[j] += counts[i * dSqrt + j];
        }

        for (int j = 0; j < dSqrt; j++) {
            buckets.add(new int[prefixSums[j]]);
            bucketSizes[j] = prefixSums[j];
        }

        int[] offsets = new int[dSqrt];
        for (int i = 0; i < p; i++) {
            int start = i * (n / p);
            int size = i == p - 1 ? n - start : n / p;
            for (int j = 0; j < size; j++) {
                int elem = arr[start + j];
                int bucket = 0;
                while (bucket < dSqrt - 1 && elem > pivots[bucket]) bucket++;
                buckets.get(bucket)[offsets[bucket]++] = elem;
            }
        }
    }

    static void pemDistSort(int[] arr, int p, int m, int b, int d) {
        int n = arr.length;
        if (n <= m) {
            quicksort(arr, 0, n - 1);
            return;
        }

        int dSqrt = (int)Math.sqrt(d);
        int segmentSize = n / p;

        System.out.println("Segments:");
        for (int i = 0; i < p; i++) {
            int size = i == p - 1 ? n - i * segmentSize : segmentSize;
            System.out.print("Segment " + i + ": ");
            for (int j = 0; j < size; j++) System.out.print(arr[i * segmentSize + j] + " ");
            System.out.println();
        }

        int[] pivots = new int[dSqrt - 1];
        for (int j = 0; j < dSqrt - 1; j++) {
            pivots[j] = pemSelect(arr, n, (j + 1) * n / dSqrt);
        }
        System.out.print("Pivots: ");
        for (int x : pivots) System.out.print(x + " ");
        System.out.println();

        List<int[]> buckets = new ArrayList<>();
        int[] bucketSizes = new int[dSqrt];
        pemMultipartition(arr, n, pivots, dSqrt, p, buckets, bucketSizes);

        System.out.println("Buckets:");
        for (int j = 0; j < dSqrt; j++) {
            System.out.print("Bucket " + j + ": ");
            for (int x : buckets.get(j)) System.out.print(x + " ");
            System.out.println();
        }

        int[] output = new int[n];
        int outputPos = 0;
        for (int j = 0; j < dSqrt; j++) {
            int processors = (int)Math.ceil((double)bucketSizes[j] / (n / p));
            pemDistSort(buckets.get(j), processors, m, b, d);
            System.arraycopy(buckets.get(j), 0, output, outputPos, bucketSizes[j]);
            outputPos += bucketSizes[j];
        }

        System.arraycopy(output, 0, arr, 0, n);
    }

    public static void main(String[] args) {
        int[] arr = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
        int n = arr.length, p = 4, m = 8, b = 2, d = 4;

        System.out.print("Initial array: ");
        for (int x : arr) System.out.print(x + " ");
        System.out.println();

        pemDistSort(arr, p, m, b, d);

        System.out.print("Sorted array: ");
        for (int x : arr) System.out.print(x + " ");
        System.out.println();
    }
}
