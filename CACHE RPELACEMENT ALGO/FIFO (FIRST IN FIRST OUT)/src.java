import java.util.*;

public class FIFOCache {
    private int capacity;
    private LinkedList<Integer> order;
    private HashMap<Integer, Integer> cache;

    public FIFOCache(int capacity) {
        this.capacity = capacity;
        this.order = new LinkedList<>();
        this.cache = new HashMap<>();
    }

    public int get(int key) {
        if (!cache.containsKey(key)) return -1;
        System.out.print("Cache after get(" + key + "): [");
        int i = 0;
        for (Integer k : order) {
            System.out.print("(" + k + ", " + cache.get(k) + ")" + (i++ < order.size() - 1 ? ", " : ""));
        }
        System.out.println("]");
        return cache.get(key);
    }

    public void put(int key, int value) {
        if (cache.containsKey(key)) {
            cache.put(key, value);
        } else {
            if (cache.size() == capacity) {
                int oldest = order.removeFirst();
                cache.remove(oldest);
            }
            cache.put(key, value);
            order.add(key);
        }
        System.out.print("Cache after put(" + key + ", " + value + "): [");
        int i = 0;
        for (Integer k : order) {
            System.out.print("(" + k + ", " + cache.get(k) + ")" + (i++ < order.size() - 1 ? ", " : ""));
        }
        System.out.println("]");
    }

    public static void main(String[] args) {
        FIFOCache cache = new FIFOCache(3);
        cache.put(1, 10);
        cache.put(2, 20);
        cache.put(3, 30);
        cache.put(4, 40);
        System.out.println("Get(2) = " + cache.get(2));
        cache.put(5, 50);
    }
}
