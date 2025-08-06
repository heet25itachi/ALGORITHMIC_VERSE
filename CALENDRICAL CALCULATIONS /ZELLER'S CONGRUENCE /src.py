weekdays = ["Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
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

def get_zeller(day, month, year):
    if month == 1 or month == 2:
        month += 12
        year -= 1
    K = year % 100
    J = year // 100
    return (day + ((13 * (month + 1)) // 5) + K + (K // 4) + (J // 4) + 5 * J) % 7

def get_weekday(day, month, year):
    if not is_valid_date(day, month, year):
        return "Invalid date"
    return weekdays[get_zeller(day, month, year)]

if __name__ == "__main__":
    tests = [(1, 1, 2000), (1, 3, 2000), (18, 9, 1985), (12, 4, 1861)]
    for day, month, year in tests:
        print(f"{month_names[month - 1]} {day}, {year} is a {get_weekday(day, month, year)}")
