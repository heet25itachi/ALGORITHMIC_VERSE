function liang_barsky
    function [accept, p0, p1] = liang_barsky_clip(xmin, ymin, xmax, ymax, p0, p1)
        if xmin >= xmax || ymin >= ymax
            accept = false;
            return;
        end

        dx = p1(1) - p0(1);
        dy = p1(2) - p0(2);
        p = [-dx, dx, -dy, dy];
        q = [p0(1) - xmin, xmax - p0(1), p0(2) - ymin, ymax - p0(2)];
        u1 = 0; u2 = 1;

        for i = 1:4
            if p(i) == 0 && q(i) < 0
                accept = false;
                return;
            end
            if p(i) ~= 0
                t = q(i) / p(i);
                if p(i) < 0
                    if t > u1
                        u1 = t;
                    end
                else
                    if t < u2
                        u2 = t;
                    end
                end
            end
        end
        if u1 > u2
            accept = false;
            return;
        end

        p0_new = [p0(1) + u1 * dx, p0(2) + u1 * dy];
        p1_new = [p0(1) + u2 * dx, p0(2) + u2 * dy];
        p0 = p0_new;
        p1 = p1_new;
        accept = true;
    end

    xmin = 0; ymin = 0; xmax = 10; ymax = 10;
    tests = [2, 2, 8, 8; 12, 12, 15, 15; 5, 12, 15, 5; -5, 5, 15, 5];
    for i = 1:size(tests, 1)
        p0 = tests(i, 1:2);
        p1 = tests(i, 3:4);
        fprintf('Line from (%.1f, %.1f) to (%.1f, %.1f): ', p0(1), p0(2), p1(1), p1(2));
        [accept, p0_new, p1_new] = liang_barsky_clip(xmin, ymin, xmax, ymax, p0, p1);
        if accept
            fprintf('Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n', p0_new(1), p0_new(2), p1_new(1), p1_new(2));
        else
            fprintf('Rejected\n');
        end
    end
end
