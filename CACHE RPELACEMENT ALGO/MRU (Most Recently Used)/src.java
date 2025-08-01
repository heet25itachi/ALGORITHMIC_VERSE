import java.util.*;

public class MRUCache {
    private int capacity;
    private LinkedHashMap<Integer, Integer> cache;

    public MRUCache(int capacity) {
        this.capacity = capacity;
        this.cache = new LinkedHashMap<>();
    }

    public int get(int key) {
        if (!cache.containsKey(key)) return -1;
        int value = cache.get(key);
        cache.remove(key);
        cache.put(key, value);
        System.out.print("Cache after get(" + key + "): [");
        int i = 0;
        for (Map.Entry<Integer, Integer> entry : cache.entrySet()) {
            System.out.print("(" + entry.getKey() + ", " + entry.getValue() + ")" + (i++ < cache.size() - 1 ? ", " : ""));
        }
        System.out.println("]");
        return value;
    }

    public void put(int key, int value) {
        if (cache.containsKey(key)) {
            cache.remove(key);
        } else if (cache.size() == capacity) {
            Iterator<Integer> it = cache.keySet().iterator();
            it.next();
            it.remove();
        }
        cache.put(key, value);
        System.out.print("Cache after put(" + key + ", " + value + "): [");
        int i = 0;
        for (Map.Entry<Integer, Integer> entry : cache.entrySet()) {
            System.out.print("(" + entry.getKey() + ", " + entry.getValue() + ")" + (i++ < cache.size() - 1 ? ", " : ""));
        }
        System.out.println("]");
    }

    public static void main(String[] args) {
        MRUCache cache = new MRUCache(3);
        cache.put(1, 10);
        cache.put(2, 20);
        cache.put(3, 30);
        cache.put(4, 40);
        System.out.println("Get(2) = " + cache.get(2));
        cache.put(5, 50);
    }
}
