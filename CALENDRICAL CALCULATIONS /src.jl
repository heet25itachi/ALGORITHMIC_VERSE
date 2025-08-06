function get_month_index(month::Int)
    indices = [11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    return 1 <= month <= 12 ? indices[month] : -1
end

function get_month_name(month::Int)
    names = ["January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December"]
    return 1 <= month <= 12 ? names[month] : "Invalid"
end

function days_in_month(month::Int, year::Int)
    if month < 1 || month > 12 || year < 1753
        return -1
    end
    m = Float64(get_month_index(month))
    y = Float64(year)

    d = 30.0 + floor(0.6 * m + 0.4) - floor(0.6 * m - 0.2) - 2.0 * floor(m / 12.0)
    if m == 12
        d += floor((y - 1) / 4.0 - floor((y - 1) / 4.0) + 0.25)
        if year % 100 == 0
            century_term = floor(0.3 + (floor(y / 100.0) - 3) / 4.5 - floor((floor(y / 100.0) - 3) / 4.5))
            d += floor((century_term + 99.0 + 100.0 * (y / 100.0 - floor(y / 100.0))) / 100.0) - 1.0
        end
    end
    return Int(d)
end

tests = [(2, 2000), (3, 2023), (4, 2024), (2, 1900)]
for (month, year) in tests
    days = days_in_month(month, year)
    if days != -1
        println("Days in $(get_month_name(month)) $year: $days")
    else
        println("Invalid input")
    end
end
