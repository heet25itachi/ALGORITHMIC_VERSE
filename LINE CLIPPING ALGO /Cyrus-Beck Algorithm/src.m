function cyrus_beck
    vertices = [0, 0; 10, 0; 10, 10; 0, 10];
    n_vertices = 4;

    function normal = compute_normal(i)
        v1 = vertices(i, :);
        v2 = vertices(mod(i, n_vertices) + 1, :);
        normal = [-(v2(2) - v1(2)), v2(1) - v1(1)];
    end

    function dp = dot_product(a, b)
        dp = a(1) * b(1) + a(2) * b(2);
    end

    function [accept, p0, p1] = cyrus_beck_clip(p0, p1)
        D = [p1(1) - p0(1), p1(2) - p0(2)];
        if D(1) == 0 && D(2) == 0
            accept = false;
            return;
        end

        tE = 0; tL = 1;
        for i = 1:n_vertices
            normal = compute_normal(i);
            PE = vertices(i, :);
            diff = [p0(1) - PE(1), p0(2) - PE(2)];
            num = -dot_product(normal, diff);
            den = dot_product(normal, D);
            if den == 0
                continue;
            end
            t = num / den;
            if den > 0
                if t < tL
                    tL = t;
                end
            else
                if t > tE
                    tE = t;
                end
            end
        end
        if tE > tL || tE < 0 || tE > 1 || tL < 0 || tL > 1
            accept = false;
            return;
        end

        p0_new = [p0(1) + tE * D(1), p0(2) + tE * D(2)];
        p1_new = [p0(1) + tL * D(1), p0(2) + tL * D(2)];
        p0 = p0_new;
        p1 = p1_new;
        accept = true;
    end

    tests = [2, 2, 8, 8; 12, 12, 15, 15; 5, 12, 15, 5; -5, 5, 15, 5];
    for i = 1:size(tests, 1)
        p0 = tests(i, 1:2);
        p1 = tests(i, 3:4);
        fprintf('Line from (%.1f, %.1f) to (%.1f, %.1f): ', p0(1), p0(2), p1(1), p1(2));
        [accept, p0_new, p1_new] = cyrus_beck_clip(p0, p1);
        if accept
            fprintf('Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n', p0_new(1), p0_new(2), p1_new(1), p1_new(2));
        else
            fprintf('Rejected\n');
        end
    end
end
