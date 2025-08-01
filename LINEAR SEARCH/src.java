public class LinearSearch {
    public static int linearSearch(int[] arr, int target) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == target) {
                return i;
            }
        }
        return -1;
    }

    public static void main(String[] args) {
        int[] arr = {3, 7, 1, 9, 4};
        int target = 9;
        int result = linearSearch(arr, target);
        System.out.println("Target " + target + " found at index: " + result);
    }
}
