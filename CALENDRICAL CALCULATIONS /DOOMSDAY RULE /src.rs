const WEEKDAYS: [&str; 7] = ["Noneday", "Oneday", "Twosday", "Treblesday", "Foursday", "Fiveday", "Six-a-day"];
const DOOMSDAY_DATES: [[i32; 2]; 12] = [[3, 4], [28, 29], [14, 14], [4, 4], [9, 9], [6, 6], [11, 11], [8, 8], [5, 5], [10, 10], [7, 7], [12, 12]];
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

fn get_doomsday(year: i32) -> i32 {
    let c = year / 100;
    let y = year % 100;
    let anchor = (5 * (c % 4) + 2) % 7;
    let a = y / 12;
    let b = y % 12;
    let c_y = b / 4;
    (anchor + a + b + c_y) % 7
}

fn get_weekday(day: i32, month: i32, year: i32) -> &'static str {
    if !is_valid_date(day, month, year) {
        return "Invalid date";
    }
    let doomsday = get_doomsday(year);
    let ref_day = DOOMSDAY_DATES[(month - 1) as usize][if is_leap_year(year) && month <= 2 { 1 } else { 0 } as usize];
    let diff = (day - ref_day) % 7;
    let diff = if diff < 0 { diff + 7 } else { diff };
    WEEKDAYS[((doomsday + diff) % 7) as usize]
}

fn main() {
    let tests = [(18, 9, 1985), (12, 4, 1861), (25, 12, 2021), (7, 8, 1966)];
    for &(day, month, year) in tests.iter() {
        println!("{} {}, {} is a {}", MONTH_NAMES[(month - 1) as usize], day, year, get_weekday(day, month, year));
    }
}
