class LFUCache
  def initialize(capacity)
    @capacity = capacity
    @cache = {}
    @freq = {}
    @order = []
  end

  def get(key)
    return -1 unless @cache.key?(key)
    @freq[key] += 1
    print "Cache after get(#{key}): ["
    @order.each_with_index do |k, i|
      print "(#{k}, #{@cache[k]})" + (i < @order.size - 1 ? ", " : "")
    end
    puts "]"
    @cache[key]
  end

  def put(key, value)
    if @cache.key?(key)
      @cache[key] = value
      @freq[key] += 1
    else
      if @cache.size == @capacity
        min_freq = @freq.values.min
        min_key = @order.find { |k| @freq[k] == min_freq }
        @cache.delete(min_key)
        @freq.delete(min_key)
        @order.delete(min_key)
      end
      @cache[key] = value
      @freq[key] = 1
      @order << key
    end
    print "Cache after put(#{key}, #{value}): ["
    @order.each_with_index do |k, i|
      print "(#{k}, #{@cache[k]})" + (i < @order.size - 1 ? ", " : "")
    end
    puts "]"
  end
end

cache = LFUCache.new(3)
cache.put(1, 10)
cache.put(2, 20)
cache.put(3, 30)
cache.put(4, 40)
puts "Get(2) = #{cache.get(2)}"
cache.put(5, 50)
