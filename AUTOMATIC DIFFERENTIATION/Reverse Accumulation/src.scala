case class Node(value: Double, var grad: Double = 0.0, children: List[Node] = Nil, opType: String)

def backward(fNode: Node): Unit = {
    var stack = List(fNode)
    fNode.grad = 1.0

    while (stack.nonEmpty) {
        val curr = stack.head
        stack = stack.tail
        curr.opType match {
            case "mul" =>
                val left = curr.children(0)
                val right = curr.children(1)
                left.grad += curr.grad * right.value
                right.grad += curr.grad * left.value
                stack = left :: right :: stack
            case "sin" =>
                val child = curr.children(0)
                child.grad += curr.grad * math.cos(child.value)
                stack = child :: stack
            case _ => // var node, no action
        }
    }
}

val x = 3.14
val xNode = Node(x, opType = "var")
val x2Node = Node(x * x, opType = "mul", children = List(xNode, xNode))
val sinXNode = Node(math.sin(x), opType = "sin", children = List(xNode))
val fNode = Node(x2Node.value * sinXNode.value, opType = "mul", children = List(x2Node, sinXNode))

backward(fNode)
println(f"f(x) = ${fNode.value}%.2f, f'(x) = ${xNode.grad}%.2f")
