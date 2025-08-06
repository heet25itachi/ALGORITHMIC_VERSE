use strict;
use warnings;

sub liang_barsky_clip {
    my ($xmin, $ymin, $xmax, $ymax, $x0, $y0, $x1, $y1) = @_;
    return (0, $x0, $y0, $x1, $y1) if $xmin >= $xmax || $ymin >= $ymax;

    my $dx = $x1 - $x0;
    my $dy = $y1 - $y0;
    my @p = (-$dx, $dx, -$dy, $dy);
    my @q = ($x0 - $xmin, $xmax - $x0, $y0 - $ymin, $ymax - $y0);
    my $u1 = 0;
    my $u2 = 1;

    for my $i (0..3) {
        if ($p[$i] == 0 && $q[$i] < 0) {
            return (0, $x0, $y0, $x1, $y1);
        }
        if ($p[$i] != 0) {
            my $t = $q[$i] / $p[$i];
            if ($p[$i] < 0) {
                $u1 = $t if $t > $u1;
            } else {
                $u2 = $t if $t < $u2;
            }
        }
    }
    return (0, $x0, $y0, $x1, $y1) if $u1 > $u2;

    my $x0_new = $x0 + $u1 * $dx;
    my $y0_new = $y0 + $u1 * $dy;
    my $x1_new = $x0 + $u2 * $dx;
    my $y1_new = $y0 + $u2 * $dy;
    return (1, $x0_new, $y0_new, $x1_new, $y1_new);
}

my $xmin = 0;
my $ymin = 0;
my $xmax = 10;
my $ymax = 10;
my @tests = ([2, 2, 8, 8], [12, 12, 15, 15], [5, 12, 15, 5], [-5, 5, 15, 5]);
foreach my $test (@tests) {
    my ($x0, $y0, $x1, $y1) = @$test;
    printf "Line from (%.1f, %.1f) to (%.1f, %.1f): ", $x0, $y0, $x1, $y1;
    my ($accept, $x0_new, $y0_new, $x1_new, $y1_new) = liang_barsky_clip($xmin, $ymin, $xmax, $ymax, $x0, $y0, $x1, $y1);
    if ($accept) {
        printf "Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", $x0_new, $y0_new, $x1_new, $y1_new;
    } else {
        print "Rejected\n";
    }
}
