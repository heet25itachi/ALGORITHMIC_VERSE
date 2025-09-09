R = 65536

def birthday_attack
  table = {}
  trials = 0
  loop do
    x = rand(2**64)
    h = x % R
    trials += 1
    if table[h] && table[h] != x
      puts "Collision found after #{trials} trials: x_i = #{table[h]}, x_j = #{x}, hash = #{h}"
      break
    end
    table[h] = x
  end
end

birthday_attack
