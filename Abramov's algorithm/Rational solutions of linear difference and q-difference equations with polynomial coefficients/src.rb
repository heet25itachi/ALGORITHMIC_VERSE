def y(k, steps)
  result = 1.0 # y(0) = 1
  (0...steps).each do |i|
    result *= i # y(k+1) = k * y(k)
  end
  result
end

k = 5
puts "Numerical solution for y(#{k}) = #{y(k, k)}"
