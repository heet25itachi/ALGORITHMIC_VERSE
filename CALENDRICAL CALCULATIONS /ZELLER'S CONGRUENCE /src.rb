module ZellersCongruence
  WEEKDAYS = %w[Saturday Sunday Monday Tuesday Wednesday Thursday Friday]
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

  def self.get_zeller(day, month, year)
    if month == 1 || month == 2
      month += 12
      year -= 1
    end
    K = year % 100
    J = year / 100
    (day + ((13 * (month + 1)) / 5) + K + (K / 4) + (J / 4) + 5 * J) % 7
  end

  def self.get_weekday(day, month, year)
    return "Invalid date" unless is_valid_date(day, month, year)
    WEEKDAYS[get_zeller(day, month, year)]
  end
end

tests = [[1, 1, 2000], [1, 3, 2000], [18, 9, 1985], [12, 4, 1861]]
tests.each do |day, month, year|
  puts "#{ZellersCongruence::MONTH_NAMES[month - 1]} #{day}, #{year} is a #{ZellersCongruence.get_weekday(day, month, year)}"
end 
