class LRUCache
  def initialize(capacity)
    @capacity = capacity
    @cache = {}
    @order = []
  end

  def get(key)
    return -1 unless @cache.key?(key)
    @order.delete(key)
    @order << key
    print "Cache after get(#{key}): ["
    @order.each_with_index do |k, i|
      print "(#{k}, #{@cache[k]})" + (i < @order.size - 1 ? ", " : "")
    end
    puts "]"
    @cache[key]
  end

  def put(key, value)
    if @cache.key?(key)
      @order.delete(key)
    elsif @cache.size == @capacity
      @cache.delete(@order.shift)
    end
    @cache[key] = value
    @order << key
    print "Cache after put(#{key}, #{value}): ["
    @order.each_with_index do |k, i|
      print "(#{k}, #{@cache[k]})" + (i < @order.size - 1 ? ", " : "")
    end
    puts "]"
  end
end

cache = LRUCache.new(3)
cache.put(1, 10)
cache.put(2, 20)
cache.put(3, 30)
cache.put(4, 40)
puts "Get(2) = #{cache.get(2)}"
cache.put(5, 50)
