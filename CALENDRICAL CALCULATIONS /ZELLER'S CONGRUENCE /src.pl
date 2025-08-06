use strict;
use warnings;
use POSIX qw(floor);

my @weekdays = qw(Saturday Sunday Monday Tuesday Wednesday Thursday Friday);
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

sub get_zeller {
    my ($day, $month, $year) = @_;
    if ($month == 1 || $month == 2) {
        $month += 12;
        $year--;
    }
    my $K = $year % 100;
    my $J = int($year / 100);
    return ($day + int((13 * ($month + 1)) / 5) + $K + int($K / 4) + int($J / 4) + 5 * $J) % 7;
}

sub get_weekday {
    my ($day, $month, $year) = @_;
    return "Invalid date" unless is_valid_date($day, $month, $year);
    return $weekdays[get_zeller($day, $month, $year)];
}

my @tests = ([1, 1, 2000], [1, 3, 2000], [18, 9, 1985], [12, 4, 1861]);
foreach my $test (@tests) {
    my ($day, $month, $year) = @$test;
    print "$month_names[$month - 1] $day, $year is a ", get_weekday($day, $month, $year), "\n";
} 
