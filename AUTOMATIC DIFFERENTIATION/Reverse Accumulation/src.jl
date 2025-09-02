mutable struct Node
    value::Float64
    grad::Float64
    children::Vector{Node}
    op_type::String
    Node(value, op_type, children=Node[]) = new(value, 0.0, children, op_type)
end

function backward(f_node::Node)
    stack = [f_node]
    f_node.grad = 1.0

    while !isempty(stack)
        curr = pop!(stack)
        if curr.op_type == "mul"
            left, right = curr.children
            left.grad += curr.grad * right.value
            right.grad += curr.grad * left.value
            append!(stack, [left, right])
        elseif curr.op_type == "sin"
            child = curr.children[1]
            child.grad += curr.grad * cos(child.value)
            push!(stack, child)
        end
    end
end

x = 3.14
x_node = Node(x, "var")
x2_node = Node(x * x, "mul", [x_node, x_node])
sin_x_node = Node(sin(x), "sin", [x_node])
f_node = Node(x2_node.value * sin_x_node.value, "mul", [x2_node, sin_x_node])

backward(f_node)
println("f(x) = ", round(f_node.value, digits=2), ", f'(x) = ", round(x_node.grad, digits=2))
