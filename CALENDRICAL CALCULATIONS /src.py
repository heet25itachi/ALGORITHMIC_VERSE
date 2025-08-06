import math

def get_month_index(month):
    indices = [11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    return indices[month - 1] if 1 <= month <= 12 else -1

def get_month_name(month):
    names = ["January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December"]
    return names[month - 1] if 1 <= month <= 12 else "Invalid"

def days_in_month(month, year):
    if month < 1 or month > 12 or year < 1753:
        return -1
    m = get_month_index(month)
    y = year

    d = 30 + math.floor(0.6 * m + 0.4) - math.floor(0.6 * m - 0.2) - 2 * math.floor(m / 12)
    if m == 12:
        d += math.floor((y - 1) / 4 - math.floor((y - 1) / 4) + 0.25)
        if year % 100 == 0:
            century_term = math.floor(0.3 + (math.floor(y / 100) - 3) / 4.5 - math.floor((math.floor(y / 100) - 3) / 4.5))
            d += math.floor((century_term + 99 + 100 * (y / 100 - math.floor(y / 100))) / 100) - 1
    return int(d)

if __name__ == "__main__":
    tests = [(2, 2000), (3, 2023), (4, 2024), (2, 1900)]
    for month, year in tests:
        days = days_in_month(month, year)
        if days != -1:
            print(f"Days in {get_month_name(month)} {year}: {days}")
        else:
            print("Invalid input")
