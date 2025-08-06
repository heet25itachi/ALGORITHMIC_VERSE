use strict;
use warnings;
use POSIX qw(floor);

sub get_month_index {
    my ($month) = @_;
    my @indices = (11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    return ($month >= 1 && $month <= 12) ? $indices[$month - 1] : -1;
}

sub get_month_name {
    my ($month) = @_;
    my @names = qw(January February March April May June July August September October November December);
    return ($month >= 1 && $month <= 12) ? $names[$month - 1] : "Invalid";
}

sub days_in_month {
    my ($month, $year) = @_;
    return -1 if $month < 1 || $month > 12 || $year < 1753;
    my $m = get_month_index($month);
    my $y = $year;

    my $d = 30 + floor(0.6 * $m + 0.4) - floor(0.6 * $m - 0.2) - 2 * floor($m / 12);
    if ($m == 12) {
        $d += floor(($y - 1) / 4 - floor(($y - 1) / 4) + 0.25);
        if ($year % 100 == 0) {
            my $century_term = floor(0.3 + (floor($y / 100) - 3) / 4.5 - floor((floor($y / 100) - 3) / 4.5));
            $d += floor(($century_term + 99 + 100 * ($y / 100 - floor($y / 100))) / 100) - 1;
        }
    }
    return $d;
}

my @tests = ([2, 2000], [3, 2023], [4, 2024], [2, 1900]);
foreach my $test (@tests) {
    my ($month, $year) = @$test;
    my $days = days_in_month($month, $year);
    if ($days != -1) {
        print "Days in ", get_month_name($month), " $year: $days\n";
    } else {
        print "Invalid input\n";
    }
}
