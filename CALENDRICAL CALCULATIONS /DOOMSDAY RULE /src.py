weekdays = ["Noneday", "Oneday", "Twosday", "Treblesday", "Foursday", "Fiveday", "Six-a-day"]
doomsday_dates = [[3, 4], [28, 29], [14, 14], [4, 4], [9, 9], [6, 6], [11, 11], [8, 8], [5, 5], [10, 10], [7, 7], [12, 12]]
month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

def is_leap_year(year):
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)

def is_valid_date(day, month, year):
    if month < 1 or month > 12 or day < 1 or year < 1583:
        return False
    max_days = month_days[month - 1]
    if month == 2 and is_leap_year(year):
        max_days = 29
    return day <= max_days

def get_doomsday(year):
    c = year // 100
    y = year % 100
    anchor = (5 * (c % 4) + 2) % 7
    a = y // 12
    b = y % 12
    c_y = b // 4
    return (anchor + a + b + c_y) % 7

def get_weekday(day, month, year):
    if not is_valid_date(day, month, year):
        return "Invalid date"
    doomsday = get_doomsday(year)
    ref_day = doomsday_dates[month - 1][1 if is_leap_year(year) and month <= 2 else 0]
    diff = (day - ref_day) % 7
    if diff < 0:
        diff += 7
    return weekdays[(doomsday + diff) % 7]

if __name__ == "__main__":
    tests = [(18, 9, 1985), (12, 4, 1861), (25, 12, 2021), (7, 8, 1966)]
    for day, month, year in tests:
        print(f"{month_names[month - 1]} {day}, {year} is a {get_weekday(day, month, year)}")
