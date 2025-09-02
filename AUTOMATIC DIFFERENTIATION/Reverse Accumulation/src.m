classdef Node < handle
    properties
        value
        grad
        children
        opType
    end
    methods
        function obj = Node(value, opType, children)
            obj.value = value;
            obj.grad = 0.0;
            obj.children = children;
            obj.opType = opType;
        end
    end
end

function backward(fNode)
    stack = {fNode};
    fNode.grad = 1.0;

    while ~isempty(stack)
        curr = stack{end};
        stack(end) = [];
        if strcmp(curr.opType, 'mul')
            left = curr.children{1};
            right = curr.children{2};
            left.grad = left.grad + curr.grad * right.value;
            right.grad = right.grad + curr.grad * left.value;
            stack = [stack, left, right];
        elseif strcmp(curr.opType, 'sin')
            child = curr.children{1};
            child.grad = child.grad + curr.grad * cos(child.value);
            stack = [stack, child];
        end
    end
end

x = 3.14;
xNode = Node(x, 'var', {});
x2Node = Node(x * x, 'mul', {xNode, xNode});
sinXNode = Node(sin(x), 'sin', {xNode});
fNode = Node(x2Node.value * sinXNode.value, 'mul', {x2Node, sinXNode});

backward(fNode);
disp(['f(x) = ', num2str(fNode.value, '%.2f'), ', f''(x) = ', num2str(xNode.grad, '%.2f')]);
