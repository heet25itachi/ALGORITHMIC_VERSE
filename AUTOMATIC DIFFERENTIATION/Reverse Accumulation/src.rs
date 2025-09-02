use std::rc::Rc;

#[derive(Clone)]
struct Node {
    value: f64,
    grad: f64,
    children: Vec<Rc<Node>>,
    op_type: String,
}

impl Node {
    fn new(value: f64, op_type: &str, children: Vec<Rc<Node>>) -> Rc<Node> {
        Rc::new(Node { value, grad: 0.0, children, op_type: op_type.to_string() })
    }
}

fn backward(f_node: Rc<Node>) {
    let mut stack = vec![f_node.clone()];
    f_node.grad = 1.0;

    while let Some(curr) = stack.pop() {
        if curr.op_type == "mul" {
            let left = curr.children[0].clone();
            let right = curr.children[1].clone();
            left.grad += curr.grad * right.value;
            right.grad += curr.grad * left.value;
            stack.push(left);
            stack.push(right);
        } else if curr.op_type == "sin" {
            let child = curr.children[0].clone();
            child.grad += curr.grad * child.value.cos();
            stack.push(child);
        }
    }
}

fn main() {
    let x = 3.14;
    let x_node = Node::new(x, "var", vec![]);
    let x2_node = Node::new(x * x, "mul", vec![x_node.clone(), x_node.clone()]);
    let sin_x_node = Node::new(x.sin(), "sin", vec![x_node.clone()]);
    let f_node = Node::new(x2_node.value * sin_x_node.value, "mul", vec![x2_node, sin_x_node]);

    backward(f_node.clone());
    println!("f(x) = {:.2}, f'(x) = {:.2}", f_node.value, x_node.grad);
}
