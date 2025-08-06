const WEEKDAYS = ["Noneday", "Oneday", "Twosday", "Treblesday", "Foursday", "Fiveday", "Six-a-day"]
const DOOMSDAY_DATES = [[3, 4], [28, 29], [14, 14], [4, 4], [9, 9], [6, 6], [11, 11], [8, 8], [5, 5], [10, 10], [7, 7], [12, 12]]
const MONTH_NAMES = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
const MONTH_DAYS = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

function is_leap_year(year::Int)
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
end

function is_valid_date(day::Int, month::Int, year::Int)
    if month < 1 || month > 12 || day < 1 || year < 1583
        return false
    end
    max_days = month == 2 && is_leap_year(year) ? 29 : MONTH_DAYS[month]
    day <= max_days
end

function get_doomsday(year::Int)
    c = div(year, 100)
    y = year % 100
    anchor = (5 * (c % 4) + 2) % 7
    a = div(y, 12)
    b = y % 12
    c_y = div(b, 4)
    (anchor + a + b + c_y) % 7
end

function get_weekday(day::Int, month::Int, year::Int)
    if !is_valid_date(day, month, year)
        return "Invalid date"
    end
    doomsday = get_doomsday(year)
    ref_day = DOOMSDAY_DATES[month][is_leap_year(year) && month <= 2 ? 2 : 1]
    diff = (day - ref_day) % 7
    diff += 7 if diff < 0
    WEEKDAYS[(doomsday + diff) % 7 + 1]
end

tests = [(18, 9, 1985), (12, 4, 1861), (25, 12, 2021), (7, 8, 1966)]
for (day, month, year) in tests
    println("$(MONTH_NAMES[month]) $day, $year is a $(get_weekday(day, month, year))")
end
