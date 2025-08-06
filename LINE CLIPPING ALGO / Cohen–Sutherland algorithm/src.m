function cohen_sutherland
    INSIDE = 0; LEFT = 1; RIGHT = 2; BOTTOM = 4; TOP = 8;
    XMIN = 0; YMIN = 0; XMAX = 10; YMAX = 10;

    function code = compute_outcode(x, y)
        code = INSIDE;
        if x < XMIN
            code = bitor(code, LEFT);
        elseif x > XMAX
            code = bitor(code, RIGHT);
        end
        if y < YMIN
            code = bitor(code, BOTTOM);
        elseif y > YMAX
            code = bitor(code, TOP);
        end
    end

    function [accept, x0, y0, x1, y1] = cohen_sutherland_clip(x0, y0, x1, y1)
        outcode0 = compute_outcode(x0, y0);
        outcode1 = compute_outcode(x1, y1);
        accept = false;

        while true
            if ~bitor(outcode0, outcode1)
                accept = true;
                break;
            elseif bitand(outcode0, outcode1)
                break;
            else
                x = 0; y = 0;
                outcode_out = outcode1;
                if outcode0 > outcode1
                    outcode_out = outcode0;
                end
                if bitand(outcode_out, TOP)
                    x = x0 + (x1 - x0) * (YMAX - y0) / (y1 - y0);
                    y = YMAX;
                elseif bitand(outcode_out, BOTTOM)
                    x = x0 + (x1 - x0) * (YMIN - y0) / (y1 - y0);
                    y = YMIN;
                elseif bitand(outcode_out, RIGHT)
                    y = y0 + (y1 - y0) * (XMAX - x0) / (x1 - x0);
                    x = XMAX;
                else
                    y = y0 + (y1 - y0) * (XMIN - x0) / (x1 - x0);
                    x = XMIN;
                end
                if outcode_out == outcode0
                    x0 = x; y0 = y;
                    outcode0 = compute_outcode(x0, y0);
                else
                    x1 = x; y1 = y;
                    outcode1 = compute_outcode(x1, y1);
                end
            end
        end
    end

    tests = [2, 2, 8, 8; 12, 12, 15, 15; 5, 12, 15, 5; -5, 5, 15, 5];
    for i = 1:size(tests, 1)
        x0 = tests(i, 1); y0 = tests(i, 2); x1 = tests(i, 3); y1 = tests(i, 4);
        fprintf('Line from (%.1f, %.1f) to (%.1f, %.1f): ', x0, y0, x1, y1);
        [accept, x0, y0, x1, y1] = cohen_sutherland_clip(x0, y0, x1, y1);
        if accept
            fprintf('Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n', x0, y0, x1, y1);
        else
            fprintf('Rejected\n');
        end
    end
end
