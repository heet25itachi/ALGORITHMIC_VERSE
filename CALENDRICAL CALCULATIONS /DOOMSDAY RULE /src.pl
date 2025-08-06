use strict;
use warnings;
use POSIX qw(floor);

my @weekdays = qw(Noneday Oneday Twosday Treblesday Foursday Fiveday Six-a-day);
my @doomsday_dates = ([3, 4], [28, 29], [14, 14], [4, 4], [9, 9], [6, 6], [11, 11], [8, 8], [5, 5], [10, 10], [7, 7], [12, 12]);
my @month_names = qw(January February March April May June July August September October November December);
my @month_days = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

sub is_leap_year {
    my ($year) = @_;
    return $year % 4 == 0 && ($year % 100 != 0 || $year % 400 == 0);
}

sub is_valid_date {
    my ($day, $month, $year) = @_;
    return 0 if $month < 1 || $month > 12 || $day < 1 || $year < 1583;
    my $max_days = $month_days[$month - 1];
    $max_days = 29 if $month == 2 && is_leap_year($year);
    return $day <= $max_days;
}

sub get_doomsday {
    my ($year) = @_;
    my $c = int($year / 100);
    my $y = $year % 100;
    my $anchor = (5 * ($c % 4) + 2) % 7;
    my $a = int($y / 12);
    my $b = $y % 12;
    my $c_y = int($b / 4);
    return ($anchor + $a + $b + $c_y) % 7;
}

sub get_weekday {
    my ($day, $month, $year) = @_;
    return "Invalid date" unless is_valid_date($day, $month, $year);
    my $doomsday = get_doomsday($year);
    my $ref_day = $doomsday_dates[$month - 1][is_leap_year($year) && $month <= 2 ? 1 : 0];
    my $diff = ($day - $ref_day) % 7;
    $diff += 7 if $diff < 0;
    return $weekdays[($doomsday + $diff) % 7];
}

my @tests = ([18, 9, 1985], [12, 4, 1861], [25, 12, 2021], [7, 8, 1966]);
foreach my $test (@tests) {
    my ($day, $month, $year) = @$test;
    print "$month_names[$month - 1] $day, $year is a ", get_weekday($day, $month, $year), "\n";
}
