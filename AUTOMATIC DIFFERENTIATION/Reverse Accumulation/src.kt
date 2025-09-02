data class Node(val value: Double, var grad: Double = 0.0, val children: List<Node> = listOf(), val opType: String)

fun backward(fNode: Node) {
    val stack = ArrayDeque<Node>().apply { add(fNode) }
    fNode.grad = 1.0

    while (stack.isNotEmpty()) {
        val curr = stack.removeLast()
        when (curr.opType) {
            "mul" -> {
                val left = curr.children[0]
                val right = curr.children[1]
                left.grad += curr.grad * right.value
                right.grad += curr.grad * left.value
                stack.addAll(listOf(left, right))
            }
            "sin" -> {
                val child = curr.children[0]
                child.grad += curr.grad * Math.cos(child.value)
                stack.add(child)
            }
        }
    }
}

fun main() {
    val x = 3.14
    val xNode = Node(x, opType = "var")
    val x2Node = Node(x * x, opType = "mul", children = listOf(xNode, xNode))
    val sinXNode = Node(Math.sin(x), opType = "sin", children = listOf(xNode))
    val fNode = Node(x2Node.value * sinXNode.value, opType = "mul", children = listOf(x2Node, sinXNode))

    backward(fNode)
    println("f(x) = %.2f, f'(x) = %.2f".format(fNode.value, xNode.grad))
}
