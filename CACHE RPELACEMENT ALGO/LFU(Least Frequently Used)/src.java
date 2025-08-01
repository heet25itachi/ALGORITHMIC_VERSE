import java.util.*;

public class LFUCache {
    private int capacity;
    private HashMap<Integer, Integer> cache;
    private HashMap<Integer, Integer> freq;
    private LinkedList<Integer> order;

    public LFUCache(int capacity) {
        this.capacity = capacity;
        this.cache = new HashMap<>();
        this.freq = new HashMap<>();
        this.order = new LinkedList<>();
    }

    public int get(int key) {
        if (!cache.containsKey(key)) return -1;
        freq.put(key, freq.get(key) + 1);
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
            freq.put(key, freq.get(key) + 1);
        } else {
            if (cache.size() == capacity) {
                int min_freq = Integer.MAX_VALUE, min_key = order.getFirst();
                for (Integer k : order) {
                    if (freq.get(k) < min_freq) {
                        min_freq = freq.get(k);
                        min_key = k;
                    }
                }
                cache.remove(min_key);
                freq.remove(min_key);
                order.remove(min_key);
            }
            cache.put(key, value);
            freq.put(key, 1);
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
        LFUCache cache = new LFUCache(3);
        cache.put(1, 10);
        cache.put(2, 20);
        cache.put(3, 30);
        cache.put(4, 40);
        System.out.println("Get(2) = " + cache.get(2));
        cache.put(5, 50);
    }
}
