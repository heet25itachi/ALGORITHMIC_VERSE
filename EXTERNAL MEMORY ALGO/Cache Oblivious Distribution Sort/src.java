import java.util.*;

class Bucket {
    List<Integer> elements;
    int pivot;
    Bucket() { elements = new ArrayList<>(); pivot = (int)1e9; }
}

public class CacheObliviousSort {
    static int approximateMedian(List<Integer> arr) {
        if (arr.isEmpty()) return 0;
        int mid = arr.size() / 2;
        List<Integer> values = Arrays.asList(arr.get(0), arr.get(mid), arr.get(arr.size() - 1));
        Collections.sort(values);
        return values.get(1);
    }

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

    static void copyElems(int[] arr, int[] next, int[] bnum, List<Bucket> buckets, int subarraySize, int bucketIdx, int sqrtN) {
        while (next[bucketIdx] < subarraySize) {
            if (bnum[bucketIdx] >= buckets.size()) {
                buckets.add(new Bucket());
            }
            if (arr[next[bucketIdx]] <= buckets.get(bnum[bucketIdx]).pivot) {
                if (buckets.get(bnum[bucketIdx]).elements.size() >= 2 * sqrtN) {
                    int median = approximateMedian(buckets.get(bnum[bucketIdx]).elements);
                    Bucket newBucket = new Bucket();
                    newBucket.pivot = buckets.get(bnum[bucketIdx]).pivot;
                    buckets.get(bnum[bucketIdx]).pivot = median;
                    List<Integer> newElements = new ArrayList<>();
                    for (int x : buckets.get(bnum[bucketIdx]).elements)
                        if (x <= median) newElements.add(x);
                        else newBucket.elements.add(x);
                    buckets.get(bnum[bucketIdx]).elements = newElements;
                    buckets.add(newBucket);
                    for (int i = 0; i < bnum.length; i++)
                        if (bnum[i] > bnum[bucketIdx]) bnum[i]++;
                }
                buckets.get(bnum[bucketIdx]).elements.add(arr[next[bucketIdx]]);
                next[bucketIdx]++;
            } else {
                bnum[bucketIdx]++;
            }
        }
    }

    static void distribute(int[] arr, int[] next, int[] bnum, int i, int j, int m, List<Bucket> buckets, int sqrtN) {
        if (m == 1) {
            copyElems(arr, next, bnum, buckets, sqrtN, i, sqrtN);
        } else {
            distribute(arr, next, bnum, i, j, m / 2, buckets, sqrtN);
            distribute(arr, next, bnum, i + m / 2, j, m / 2, buckets, sqrtN);
            distribute(arr, next, bnum, i, j + m / 2, m / 2, buckets, sqrtN);
            distribute(arr, next, bnum, i + m / 2, j + m / 2, m / 2, buckets, sqrtN);
        }
    }

    static void cacheObliviousSort(int[] arr) {
        int n = arr.length;
        if (n <= 1) return;
        int sqrtN = (int)Math.sqrt(n);
        if (sqrtN * sqrtN != n) return;

        // Step 1: Partition and sort subarrays
        for (int i = 0; i < sqrtN; i++)
            quicksort(arr, i * sqrtN, i * sqrtN + sqrtN - 1);

        // Step 2: Distribute
        int[] next = new int[sqrtN], bnum = new int[sqrtN];
        List<Bucket> buckets = new ArrayList<>();
        buckets.add(new Bucket());
        distribute(arr, next, bnum, 0, 0, sqrtN, buckets, sqrtN);

        // Step 3: Sort buckets
        for (Bucket bucket : buckets)
            bucket.elements.sort(null);

        // Step 4: Concatenate
        int k = 0;
        for (Bucket bucket : buckets)
            for (int x : bucket.elements)
                arr[k++] = x;
    }

    public static void main(String[] args) {
        int[] arr = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
        System.out.print("Initial array: ");
        for (int x : arr) System.out.print(x + " ");
        System.out.println();
        cacheObliviousSort(arr);
        System.out.print("Sorted array: ");
        for (int x : arr) System.out.print(x + " ");
        System.out.println();
    }
}
