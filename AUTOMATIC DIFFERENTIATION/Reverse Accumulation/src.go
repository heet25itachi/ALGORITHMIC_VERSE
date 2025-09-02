package main

import (
    "fmt"
    "math"
)

type Node struct {
    value    float64
    grad     float64
    children []*Node
    opType   string
}

func backward(fNode *Node) {
    stack := []*Node{fNode}
    fNode.grad = 1.0

    for len(stack) > 0 {
        curr := stack[len(stack)-1]
        stack = stack[:len(stack)-1]
        if curr.opType == "mul" {
            left, right := curr.children[0], curr.children[1]
            left.grad += curr.grad * right.value
            right.grad += curr.grad * left.value
            stack = append(stack, left, right)
        } else if curr.opType == "sin" {
            child := curr.children[0]
            child.grad += curr.grad * math.Cos(child.value)
            stack = append(stack, child)
        }
    }
}

func main() {
    x := 3.14
    xNode := &Node{value: x, opType: "var"}
    x2Node := &Node{value: x * x, opType: "mul", children: []*Node{xNode, xNode}}
    sinXNode := &Node{value: math.Sin(x), opType: "sin", children: []*Node{xNode}}
    fNode := &Node{value: x2Node.value * sinXNode.value, opType: "mul", children: []*Node{x2Node, sinXNode}}

    backward(fNode)
    fmt.Printf("f(x) = %.2f, f'(x) = %.2f\n", fNode.value, xNode.grad)
}
