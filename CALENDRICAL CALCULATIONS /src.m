function calendrical_calculation()
    function idx = get_month_index(month)
        indices = [11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        if month >= 1 && month <= 12
            idx = indices(month);
        else
            idx = -1;
        end
    end

    function name = get_month_name(month)
        names = {'January', 'February', 'March', 'April', 'May', 'June', ...
                 'July', 'August', 'September', 'October', 'November', 'December'};
        if month >= 1 && month <= 12
            name = names{month};
        else
            name = 'Invalid';
        end
    end

    function d = days_in_month(month, year)
        if month < 1 || month > 12 || year < 1753
            d = -1;
            return;
        end
        m = get_month_index(month);
        y = year;

        d = 30 + floor(0.6 * m + 0.4) - floor(0.6 * m - 0.2) - 2 * floor(m / 12);
        if m == 12
            d = d + floor((y - 1) / 4 - floor((y - 1) / 4) + 0.25);
            if mod(year, 100) == 0
                century_term = floor(0.3 + (floor(y / 100) - 3) / 4.5 - floor((floor(y / 100) - 3) / 4.5));
                d = d + floor((century_term + 99 + 100 * (y / 100 - floor(y / 100))) / 100) - 1;
            end
        end
    end

    tests = [2, 2000; 3, 2023; 4, 2024; 2, 1900];
    for i = 1:size(tests, 1)
        month = tests(i, 1);
        year = tests(i, 2);
        days = days_in_month(month, year);
        if days ~= -1
            fprintf('Days in %s %d: %d\n', get_month_name(month), year, days);
        else
            fprintf('Invalid input\n');
        end
    end
end
