#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct Node {
    double value, grad;
    struct Node** children;
    int child_count;
    char* op_type;
} Node;

Node* create_node(double value, char* op_type, Node** children, int child_count) {
    Node* node = malloc(sizeof(Node));
    node->value = value;
    node->grad = 0.0;
    node->children = children;
    node->child_count = child_count;
    node->op_type = op_type;
    return node;
}

void backward(Node* f_node, Node* x_node) {
    Node** stack = malloc(100 * sizeof(Node*));
    int stack_size = 0;
    stack[stack_size++] = f_node;
    f_node->grad = 1.0;

    while (stack_size > 0) {
        Node* curr = stack[--stack_size];
        if (strcmp(curr->op_type, "mul") == 0) {
            Node* left = curr->children[0];
            Node* right = curr->children[1];
            left->grad += curr->grad * right->value;
            right->grad += curr->grad * left->value;
            stack[stack_size++] = left;
            stack[stack_size++] = right;
        } else if (strcmp(curr->op_type, "sin") == 0) {
            Node* child = curr->children[0];
            child->grad += curr->grad * cos(child->value);
            stack[stack_size++] = child;
        }
    }
    free(stack);
}

int main() {
    double x = 3.14;
    Node* x_node = create_node(x, "var", NULL, 0);
    Node* x2_children[] = {x_node, x_node};
    Node* x2_node = create_node(x * x, "mul", x2_children, 2);
    Node* sin_x_node = create_node(sin(x), "sin", &x_node, 1);
    Node* f_children[] = {x2_node, sin_x_node};
    Node* f_node = create_node(x2_node->value * sin_x_node->value, "mul", f_children, 2);

    backward(f_node, x_node);
    printf("f(x) = %.2f, f'(x) = %.2f\n", f_node->value, x_node->grad);

    free(x_node); free(x2_node); free(sin_x_node); free(f_node);
    return 0;
}
