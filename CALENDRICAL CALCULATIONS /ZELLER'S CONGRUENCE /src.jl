const WEEKDAYS = ["Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
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

function get_zeller(day::Int, month::Int, year::Int)
    m, y = month, year
    if month == 1 || month == 2
        m += 12
        y -= 1
    end
    K = y % 100
    J = div(y, 100)
    (day + div(13 * (m + 1), 5) + K + div(K, 4) + div(J, 4) + 5 * J) % 7
end

function get_weekday(day::Int, month::Int, year::Int)
    if !is_valid_date(day, month, year)
        return "Invalid date"
    end
    WEEKDAYS[get_zeller(day, month, year) + 1]
end

tests = [(1, 1, 2000), (1, 3, 2000), (18, 9, 1985), (12, 4, 1861)]
for (day, month, year) in tests
    println("$(MONTH_NAMES[month]) $day, $year is a $(get_weekday(day, month, year))")
end
