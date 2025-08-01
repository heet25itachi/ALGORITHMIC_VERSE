object LFUCache {
  class LFUCache(capacity: Int) {
    private val cache = scala.collection.mutable.Map[Int, Int]()
    private val freq = scala.collection.mutable.Map[Int, Int]()
    private val order = scala.collection.mutable.ListBuffer[Int]()

    def get(key: Int): Int = {
      if (!cache.contains(key)) return -1
      freq(key) = freq(key) + 1
      print(s"Cache after get($key): [")
      order.zipWithIndex.foreach { case (k, i) =>
        print(s"($k, ${cache(k)})" + (if (i < order.length - 1) ", " else ""))
      }
      println("]")
      cache(key)
    }

    def put(key: Int, value: Int): Unit = {
      if (cache.contains(key)) {
        cache(key) = value
        freq(key) = freq(key) + 1
      } else {
        if (cache.size == capacity) {
          val min_freq = freq.values.min
          val min_key = order.find(k => freq(k) == min_freq).get
          cache.remove(min_key)
          freq.remove(min_key)
          order -= min_key
        }
        cache(key) = value
        freq(key) = 1
        order += key
      }
      print(s"Cache after put($key, $value): [")
      order.zipWithIndex.foreach { case (k, i) =>
        print(s"($k, ${cache(k)})" + (if (i < order.length - 1) ", " else ""))
      }
      println("]")
    }
  }

  def main(args: Array[String]): Unit = {
    val cache = new LFUCache(3)
    cache.put(1, 10)
    cache.put(2, 20)
    cache.put(3, 30)
    cache.put(4, 40)
    println(s"Get(2) = ${cache.get(2)}")
    cache.put(5, 50)
  }
}
