public class JumpSearch {
    public static int jumpSearch(int[] arr, int target) {
        int size = arr.length;
        int step = (int)Math.sqrt(size);
        int prev = 0;
        while (arr[Math.min(step, size) - 1] < target) {
            prev = step;
            step += (int)Math.sqrt(size);
            if (prev >= size) return -1;
        }
        while (prev < size && arr[prev] < target) {
            prev++;
        }
        if (prev < size && arr[prev] == target) return prev;
        return -1;
    }

    public static void main(String[] args) {
        int[] arr = {1, 3, 4, 7, 9};
        int target = 9;
        int result = jumpSearch(arr, target);
        System.out.println("Target " + target + " found at index: " + result);
    }
}
