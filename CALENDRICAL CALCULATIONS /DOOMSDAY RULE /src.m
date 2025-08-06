function doomsday_rule()
    weekdays = {'Noneday', 'Oneday', 'Twosday', 'Treblesday', 'Foursday', 'Fiveday', 'Six-a-day'};
    doomsday_dates = [[3, 4]; [28, 29]; [14, 14]; [4, 4]; [9, 9]; [6, 6]; [11, 11]; [8, 8]; [5, 5]; [10, 10]; [7, 7]; [12, 12]];
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

    function d = get_doomsday(year)
        c = floor(year / 100);
        y = mod(year, 100);
        anchor = mod(5 * mod(c, 4) + 2, 7);
        a = floor(y / 12);
        b = mod(y, 12);
        c_y = floor(b / 4);
        d = mod(anchor + a + b + c_y, 7);
    end

    function w = get_weekday(day, month, year)
        if ~is_valid_date(day, month, year)
            w = 'Invalid date';
            return;
        end
        doomsday = get_doomsday(year);
        ref_day = doomsday_dates(month, 1 + (is_leap_year(year) && month <= 2));
        diff = mod(day - ref_day, 7);
        if diff < 0
            diff = diff + 7;
        end
        w = weekdays{mod(doomsday + diff, 7) + 1};
    end

    tests = [18, 9, 1985; 12, 4, 1861; 25, 12, 2021; 7, 8, 1966];
    for i = 1:size(tests, 1)
        day = tests(i, 1);
        month = tests(i, 2);
        year = tests(i, 3);
        fprintf('%s %d, %d is a %s\n', month_names{month}, day, year, get_weekday(day, month, year));
    end
end
