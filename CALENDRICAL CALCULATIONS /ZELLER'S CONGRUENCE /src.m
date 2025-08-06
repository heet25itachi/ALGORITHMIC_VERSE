function zellers_congruence()
    weekdays = {'Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'};
    month_names = {'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'};
    month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

    function leap = is_leap_year(year)
        leap = mod(year, 4) == 0 && (mod(year, 100) ~= 0 || mod(year, 400) == 0);
    end

    function valid = is_valid_date(day, month, year)
        if month < 1 || month > 12 || day < 1 || year < 1583
            valid = false;
            return;
        end
        max_days = month_days(month);
        if month == 2 && is_leap_year(year)
            max_days = 29;
        end
        valid = day <= max_days;
    end

    function h = get_zeller(day, month, year)
        if month == 1 || month == 2
            month = month + 12;
            year = year - 1;
        end
        K = mod(year, 100);
        J = floor(year / 100);
        h = mod(day + floor((13 * (month + 1)) / 5) + K + floor(K / 4) + floor(J / 4) + 5 * J, 7);
    end

    function w = get_weekday(day, month, year)
        if ~is_valid_date(day, month, year)
            w = 'Invalid date';
            return;
        end
        w = weekdays{get_zeller(day, month, year) + 1};
    end

    tests = [1, 1, 2000; 1, 3, 2000; 18, 9, 1985; 12, 4, 1861];
    for i = 1:size(tests, 1)
        day = tests(i, 1);
        month = tests(i, 2);
        year = tests(i, 3);
        fprintf('%s %d, %d is a %s\n', month_names{month}, day, year, get_weekday(day, month, year));
    end
end
