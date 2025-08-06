module DoomsdayRule
  WEEKDAYS = %w[Noneday Oneday Twosday Treblesday Foursday Fiveday Six-a-day]
  DOOMSDAY_DATES = [[3, 4], [28, 29], [14, 14], [4, 4], [9, 9], [6, 6], [11, 11], [8, 8], [5, 5], [10, 10], [7, 7], [12, 12]]
  MONTH_NAMES = %w[January February March April May June July August September October November December]
  MONTH_DAYS = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  def self.is_leap_year(year)
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
  end

  def self.is_valid_date(day, month, year)
    return false if month < 1 || month > 12 || day < 1 || year < 1583
    max_days = MONTH_DAYS[month - 1]
    max_days = 29 if month == 2 && is_leap_year(year)
    day <= max_days
  end

  def self.get_doomsday(year)
    c = year / 100
    y = year % 100
    anchor = (5 * (c % 4) + 2) % 7
    a = y / 12
    b = y % 12
    c_y = b / 4
    (anchor + a + b + c_y) % 7
  end

  def self.get_weekday(day, month, year)
    return "Invalid date" unless is_valid_date(day, month, year)
    doomsday = get_doomsday(year)
    ref_day = DOOMSDAY_DATES[month - 1][is_leap_year(year) && month <= 2 ? 1 : 0]
    diff = (day - ref_day) % 7
    diff += 7 if diff < 0
    WEEKDAYS[(doomsday + diff) % 7]
  end
end

tests = [[18, 9, 1985], [12, 4, 1861], [25, 12, 2021], [7, 8, 1966]]
tests.each do |day, month, year|
  puts "#{DoomsdayRule::MONTH_NAMES[month - 1]} #{day}, #{year} is a #{DoomsdayRule.get_weekday(day, month, year)}"
end
