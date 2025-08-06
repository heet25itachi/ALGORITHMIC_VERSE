const WEEKDAYS: [&str; 7] = ["Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"];
const MONTH_NAMES: [&str; 12] = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
const MONTH_DAYS: [i32; 12] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

fn is_leap_year(year: i32) -> bool {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

fn is_valid_date(day: i32, month: i32, year: i32) -> bool {
    if month < 1 || month > 12 || day < 1 || year < 1583 {
        return false;
    }
    let max_days = if month == 2 && is_leap_year(year) { 29 } else { MONTH_DAYS[(month - 1) as usize] };
    day <= max_days
}

fn get_zeller(day: i32, month: i32, year: i32) -> i32 {
    let (mut m, mut y) = (month, year);
    if m == 1 || m == 2 {
        m += 12;
        y -= 1;
    }
    let K = y % 100;
    let J = y / 100;
    (day + ((13 * (m + 1)) / 5) + K + (K / 4) + (J / 4) + 5 * J) % 7
}

fn get_weekday(day: i32, month: i32, year: i32) -> &'static str {
    if !is_valid_date(day, month, year) {
        return "Invalid date";
    }
    WEEKDAYS[get_zeller(day, month, year) as usize]
}

fn main() {
    let tests = [(1, 1, 2000), (1, 3, 2000), (18, 9, 1985), (12, 4, 1861)];
    for &(day, month, year) in tests.iter() {
        println!("{} {}, {} is a {}", MONTH_NAMES[(month - 1) as usize], day, year, get_weekday(day, month, year));
    }
}
