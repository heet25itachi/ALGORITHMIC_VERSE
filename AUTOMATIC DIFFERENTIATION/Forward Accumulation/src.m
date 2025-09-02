classdef Dual < handle
    properties
        value
        deriv
    end
    methods
        function obj = Dual(v, d)
            obj.value = v;
            obj.deriv = d;
        end
        function res = plus(obj1, obj2)
            res = Dual(obj1.value + obj2.value, obj1.deriv + obj2.deriv);
        end
        function res = times(obj1, obj2)
            res = Dual(obj1.value * obj2.value, obj1.deriv * obj2.value + obj1.value * obj2.deriv);
        end
    end
end

function res = sin_dual(a)
    res = Dual(sin(a.value), cos(a.value) * a.deriv);
end

x = Dual(3.14, 1.0);
x2 = x * x;
sx = sin_dual(x);
f = x2 * x2;
disp(['f(x) = ' num2str(f.value, '%.2f'), ', f'(x) = ' num2str(f.deriv, '%.2f')]);
