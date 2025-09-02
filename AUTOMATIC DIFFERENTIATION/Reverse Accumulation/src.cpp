#include <iostream>
#include <vector>
#include <string>
#include <cmath>

struct Node {
    double value, grad;
    std::vector<Node*> children;
    std::string op_type;
    Node(double v, std::string op, std::vector<Node*> c) : value(v), grad(0.0), children(c), op_type(op) {}
};

void backward(Node* f_node) {
    std::vector<Node*> stack = {f_node};
    f_node->grad = 1.0;

    while (!stack.empty()) {
        Node* curr = stack.back();
        stack.pop_back();
        if (curr->op_type == "mul") {
            Node* left = curr->children[0];
            Node* right = curr->children[1];
            left->grad += curr->grad * right->value;
            right->grad += curr->grad * left->value;
            stack.push_back(left);
            stack.push_back(right);
        } else if (curr->op_type == "sin") {
            Node* child = curr->children[0];
            child->grad += curr->grad * std::cos(child->value);
            stack.push_back(child);
        }
    }
}

int main() {
    double x = 3.14;
    Node* x_node = new Node(x, "var", {});
    Node* x2_node = new Node(x * x, "mul", {x_node, x_node});
    Node* sin_x_node = new Node(std::sin(x), "sin", {x_node});
    Node* f_node = new Node(x2_node->value * sin_x_node->value, "mul", {x2_node, sin_x_node});

    backward(f_node);
    std::cout << "f(x) = " << f_node->value << ", f'(x) = " << x_node->grad << std::endl;

    delete x_node; delete x2_node; delete sin_x_node; delete f_node;
    return 0;
}
