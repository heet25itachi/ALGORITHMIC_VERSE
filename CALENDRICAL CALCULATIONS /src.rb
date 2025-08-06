module CalendricalCalculation
  INDICES = [11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  NAMES = %w[January February March April May June July August September October November December]

  def self.get_month_index(month)
    month >= 1 && month <= 12 ? INDICES[month - 1] : -1
  end

  def self.get_month_name(month)
    month >= 1 && month <= 12 ? NAMES[month - 1] : "Invalid"
  end

  def self.days_in_month(month, year)
    return -1 if month < 1 || month > 12 || year < 1753
    m = get_month_index(month).to_f
    y = year.to_f

    d = 30 + (0.6 * m + 0.4).floor - (0.6 * m - 0.2).floor - 2 * (m / 12).floor
    if m == 12
      d += ((y - 1) / 4 - ((y - 1) / 4).floor + 0.25).floor
      if year % 100 == 0
        century_term = (0.3 + ((y / 100).floor - 3) / 4.5 - ((y / 100).floor - 3) / 4.5).floor
        d += ((century_term + 99 + 100 * (y / 100 - (y / 100).floor)) / 100).floor - 1
      end
    end
    d.to_i
  end
end

tests = [[2, 2000], [3, 2023], [4, 2024], [2, 1900]]
tests.each do |month, year|
  days = CalendricalCalculation.days_in_month(month, year)
  if days != -1
    puts "Days in #{CalendricalCalculation.get_month_name(month)} #{year}: #{days}"
  else
    puts "Invalid input"
  end
end
