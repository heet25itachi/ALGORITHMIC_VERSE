use strict;
use warnings;

use constant INSIDE => 0;
use constant LEFT   => 1;
use constant RIGHT  => 2;
use constant BOTTOM => 4;
use constant TOP    => 8;

use constant XMIN => 0.0;
use constant YMIN => 0.0;
use constant XMAX => 10.0;
use constant YMAX => 10.0;

sub compute_outcode {
    my ($x, $y) = @_;
    my $code = INSIDE;
    $code |= LEFT if $x < XMIN;
    $code |= RIGHT if $x > XMAX;
    $code |= BOTTOM if $y < YMIN;
    $code |= TOP if $y > YMAX;
    return $code;
}

sub cohen_sutherland_clip {
    my ($x0, $y0, $x1, $y1) = @_;
    my $outcode0 = compute_outcode($x0, $y0);
    my $outcode1 = compute_outcode($x1, $y1);
    my $accept = 0;

    while (1) {
        if (!($outcode0 | $outcode1)) {
            $accept = 1;
            last;
        } elsif ($outcode0 & $outcode1) {
            last;
        } else {
            my ($x, $y);
            my $outcode_out = $outcode1 > $outcode0 ? $outcode1 : $outcode0;
            if ($outcode_out & TOP) {
                $x = $x0 + ($x1 - $x0) * (YMAX - $y0) / ($y1 - $y0);
                $y = YMAX;
            } elsif ($outcode_out & BOTTOM) {
                $x = $x0 + ($x1 - $x0) * (YMIN - $y0) / ($y1 - $y0);
                $y = YMIN;
            } elsif ($outcode_out & RIGHT) {
                $y = $y0 + ($y1 - $y0) * (XMAX - $x0) / ($x1 - $x0);
                $x = XMAX;
            } else {
                $y = $y0 + ($y1 - $y0) * (XMIN - $x0) / ($x1 - $x0);
                $x = XMIN;
            }
            if ($outcode_out == $outcode0) {
                $x0 = $x; $y0 = $y;
                $outcode0 = compute_outcode($x0, $y0);
            } else {
                $x1 = $x; $y1 = $y;
                $outcode1 = compute_outcode($x1, $y1);
            }
        }
    }
    return ($accept, $x0, $y0, $x1, $y1);
}

my @tests = (
    [2, 2, 8, 8],
    [12, 12, 15, 15],
    [5, 12, 15, 5],
    [-5, 5, 15, 5]
);
foreach my $test (@tests) {
    my ($x0, $y0, $x1, $y1) = @$test;
    printf "Line from (%.1f, %.1f) to (%.1f, %.1f): ", $x0, $y0, $x1, $y1;
    my ($accept, $x0_new, $y0_new, $x1_new, $y1_new) = cohen_sutherland_clip($x0, $y0, $x1, $y1);
    if ($accept) {
        printf "Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", $x0_new, $y0_new, $x1_new, $y1_new;
    } else {
        print "Rejected\n";
    }
}
