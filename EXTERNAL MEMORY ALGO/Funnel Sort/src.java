import java.util.*;

public class Funnelsort {
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

    static void kMerger(List<int[]> inputs, int[] inputSizes, int k, int[] buffer, int bufferSize, int[] output, int[] outputSize, int k3) {
        if (k == 1) {
            int size = Math.min(inputSizes[0], k3);
            System.arraycopy(inputs.get(0), 0, output, 0, size);
            outputSize[0] = size;
            System.arraycopy(inputs.get(0), size, inputs.get(0), 0, inputSizes[0] - size);
            inputSizes[0] -= size;
            return;
        }

        int sqrtK = (int)Math.sqrt(k);
        List<int[]> inputMergers = new ArrayList<>();
        int[] inputMergerSizes = new int[sqrtK];
        int[] subBuffers = new int[sqrtK * bufferSize];
        int[] subBufferSizes = new int[sqrtK];

        for (int i = 0; i < sqrtK; i++) {
            inputMergers.add(inputs.get(i * sqrtK));
            inputMergerSizes[i] = inputSizes[i * sqrtK];
        }

        int k32 = (int)Math.pow(k, 1.5);
        for (int i = 0; i < sqrtK; i++) {
            if (subBufferSizes[i] < k32) {
                int[] tempOutput = new int[k32];
                int[] tempSize = {0};
                kMerger(inputMergers, inputMergerSizes, sqrtK, subBuffers, bufferSize, tempOutput, tempSize, k32);
                System.arraycopy(tempOutput, 0, subBuffers, i * bufferSize, tempSize[0]);
                subBufferSizes[i] = tempSize[0];
            }
        }

        List<int[]> outputMergerInputs = new ArrayList<>();
        int[] outputMergerSizes = new int[sqrtK];
        for (int i = 0; i < sqrtK; i++) {
            outputMergerInputs.add(Arrays.copyOfRange(subBuffers, i * bufferSize, (i + 1) * bufferSize));
            outputMergerSizes[i] = subBufferSizes[i];
        }

        kMerger(outputMergerInputs, outputMergerSizes, sqrtK, buffer, bufferSize, output, outputSize, k3);

        for (int i = 0; i < sqrtK; i++) {
            System.arraycopy(subBuffers, i * bufferSize + outputSize[0], subBuffers, i * bufferSize, subBufferSizes[i] - outputSize[0]);
            subBufferSizes[i] -= outputSize[0];
        }
    }

    static void funnelsort(int[] arr, int z, int l) {
        int n = arr.length;
        if (n <= z) {
            quicksort(arr, 0, n - 1);
            return;
        }

        int k = (int)Math.ceil(Math.pow(n, 1.0 / 3.0));
        int subSize = (int)Math.ceil((double)n / k);
        List<int[]> subarrays = new ArrayList<>();
        int[] subarraySizes = new int[k];

        System.out.println("Sorted subarrays:");
        for (int i = 0; i < k; i++) {
            int size = i == k - 1 ? n - i * subSize : subSize;
            int[] subarray = Arrays.copyOfRange(arr, i * subSize, i * subSize + size);
            subarraySizes[i] = size;
            quicksort(subarray, 0, size - 1);
            subarrays.add(subarray);
            System.out.print("Subarray " + i + ": ");
            for (int x : subarray) System.out.print(x + " ");
            System.out.println();
        }

        int bufferSize = 2 * (int)Math.pow(k, 1.5);
        int[] buffer = new int[bufferSize];
        int[] output = new int[n];
        int[] outputSize = {0};

        kMerger(subarrays, subarraySizes, k, buffer, bufferSize, output, outputSize, n);

        System.arraycopy(output, 0, arr, 0, n);
    }

    public static void main(String[] args) {
        int[] arr = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
        int n = arr.length, z = 8, l = 2;

        System.out.print("Initial array: ");
        for (int x : arr) System.out.print(x + " ");
        System.out.println();

        funnelsort(arr, z, l);

        System.out.print("Sorted array: ");
        for (int x : arr) System.out.print(x + " ");
        System.out.println();
    }
}
