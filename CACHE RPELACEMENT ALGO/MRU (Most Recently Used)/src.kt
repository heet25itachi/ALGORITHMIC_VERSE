class MRUCache(private val capacity: Int) {
    private val cache = mutableMapOf<Int, Int>()
    private val order = mutableListOf<Int>()

    fun get(key: Int): Int {
        if (!cache.containsKey(key)) return -1
        order.remove(key)
        order.add(0, key)
        print("Cache after get($key): [")
        order.forEachIndexed { i, k ->
            print("($k, ${cache[k]})" + if (i < order.size - 1) ", " else "")
        }
        println("]")
        return cache[key]!!
    }

    fun put(key: Int, value: Int) {
        if (cache.containsKey(key)) {
            order.remove(key)
        } else if (cache.size == capacity) {
            cache.remove(order.removeAt(0))
        }
        cache[key] = value
        order.add(0, key)
        print("Cache after put($key, $value): [")
        order.forEachIndexed { i, k ->
            print("($k, ${cache[k]})" + if (i < order.size - 1) ", " else "")
        }
        println("]")
    }
}

fun main() {
    val cache = MRUCache(3)
    cache.put(1, 10)
    cache.put(2, 20)
    cache.put(3, 30)
    cache.put(4, 40)
    println("Get(2) = ${cache.get(2)}")
    cache.put(5, 50)
}
