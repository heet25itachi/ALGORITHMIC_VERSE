import java.util.*;

class Node {
    double value, grad;
    List<Node> children;
    String opType;
    Node(double value, String opType, List<Node> children) {
        this.value = value;
        this.grad = 0.0;
        this.children = children;
        this.opType = opType;
    }
}

public class ReverseAD {
    static void backward(Node fNode) {
        Deque<Node> stack = new ArrayDeque<>();
        stack.push(fNode);
        fNode.grad = 1.0;

        while (!stack.isEmpty()) {
            Node curr = stack.pop();
            if (curr.opType.equals("mul")) {
                Node left = curr.children.get(0);
                Node right = curr.children.get(1);
                left.grad += curr.grad * right.value;
                right.grad += curr.grad * left.value;
                stack.push(left);
                stack.push(right);
            } else if (curr.opType.equals("sin")) {
                Node child = curr.children.get(0);
                child.grad += curr.grad * Math.cos(child.value);
                stack.push(child);
            }
        }
    }

    public static void main(String[] args) {
        double x = 3.14;
        Node xNode = new Node(x, "var", new ArrayList<>());
        Node x2Node = new Node(x * x, "mul", Arrays.asList(xNode, xNode));
        Node sinXNode = new Node(Math.sin(x), "sin", Arrays.asList(xNode));
        Node fNode = new Node(x2Node.value * sinXNode.value, "mul", Arrays.asList(x2Node, sinXNode));

        backward(fNode);
        System.out.printf("f(x) = %.2f, f'(x) = %.2f\n", fNode.value, xNode.grad);
    }
}
