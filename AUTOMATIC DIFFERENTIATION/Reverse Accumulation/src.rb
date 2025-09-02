class Node
  attr_accessor :value, :grad, :children, :op_type
  def initialize(value, op_type, children = [])
    @value = value
    @grad = 0.0
    @children = children
    @op_type = op_type
  end
end

def backward(f_node)
  stack = [f_node]
  f_node.grad = 1.0

  until stack.empty?
    curr = stack.pop
    case curr.op_type
    when 'mul'
      left, right = curr.children
      left.grad += curr.grad * right.value
      right.grad += curr.grad * left.value
      stack.concat([left, right])
    when 'sin'
      child = curr.children[0]
      child.grad += curr.grad * Math.cos(child.value)
      stack << child
    end
  end
end

x = 3.14
x_node = Node.new(x, 'var')
x2_node = Node.new(x * x, 'mul', [x_node, x_node])
sin_x_node = Node.new(Math.sin(x), 'sin', [x_node])
f_node = Node.new(x2_node.value * sin_x_node.value, 'mul', [x2_node, sin_x_node])

backward(f_node)
puts "f(x) = %.2f, f'(x) = %.2f" % [f_node.value, x_node.grad]
