import math

class Node:
    def __init__(self, value, op_type, children=None):
        self.value = value
        self.grad = 0.0
        self.op_type = op_type
        self.children = children if children is not None else []

def backward(f_node):
    stack = [f_node]
    f_node.grad = 1.0

    while stack:
        curr = stack.pop()
        if curr.op_type == "mul":
            left, right = curr.children
            left.grad += curr.grad * right.value
            right.grad += curr.grad * left.value
            stack.extend([left, right])
        elif curr.op_type == "sin":
            child = curr.children[0]
            child.grad += curr.grad * math.cos(child.value)
            stack.append(child)

x = 3.14
x_node = Node(x, "var")
x2_node = Node(x * x, "mul", [x_node, x_node])
sin_x_node = Node(math.sin(x), "sin", [x_node])
f_node = Node(x2_node.value * sin_x_node.value, "mul", [x2_node, sin_x_node])

backward(f_node)
print(f"f(x) = {f_node.value:.2f}, f'(x) = {x_node.grad:.2f}")
