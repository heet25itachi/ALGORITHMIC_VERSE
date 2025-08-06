use strict;
use warnings;

my @vertices = ([0, 0], [10, 0], [10, 10], [0, 10]);
my $n_vertices = 4;

sub compute_normal {
    my ($i) = @_;
    my ($v1x, $v1y) = @{$vertices[$i]};
    my ($v2x, $v2y) = @{$vertices[($i + 1) % $n_vertices]};
    return [-(($v2y - $v1y), $v2x - $v1x)];
}

sub dot_product {
    my ($a, $b) = @_;
    return $a->[0] * $b->[0] + $a->[1] * $b->[1];
}

sub cyrus_beck_clip {
    my ($p0x, $p0y, $p1x, $p1y) = @_;
    my $D = [$p1x - $p0x, $p1y - $p0y];
    return (0, $p0x, $p0y, $p1x, $p1y) if $D->[0] == 0 && $D->[1] == 0;

    my ($tE, $tL) = (0, 1);
    for my $i (0..$n_vertices-1) {
        my $normal = compute_normal($i);
        my ($PE_x, $PE_y) = @{$vertices[$i]};
        my $diff = [$p0x - $PE_x, $p0y - $PE_y];
        my $num = -dot_product($normal, $diff);
        my $den = dot_product($normal, $D);
        next if $den == 0;
        my $t = $num / $den;
        if ($den > 0) {
            $tL = $t if $t < $tL;
        } else {
            $tE = $t if $t > $tE;
        }
    }
    return (0, $p0x, $p0y, $p1x, $p1y) if $tE > $tL || $tE < 0 || $tE > 1 || $tL < 0 || $tL > 1;

    my $p0x_new = $p0x + $tE * $D->[0];
    my $p0y_new = $p0y + $tE * $D->[1];
    my $p1x_new = $p0x + $tL * $D->[0];
    my $p1y_new = $p0y + $tL * $D->[1];
    return (1, $p0x_new, $p0y_new, $p1x_new, $p1y_new);
}

my @tests = ([2, 2, 8, 8], [12, 12, 15, 15], [5, 12, 15, 5], [-5, 5, 15, 5]);
foreach my $test (@tests) {
    my ($x0, $y0, $x1, $y1) = @$test;
    printf "Line from (%.1f, %.1f) to (%.1f, %.1f): ", $x0, $y0, $x1, $y1;
    my ($accept, $p0x_new, $p0y_new, $p1x_new, $p1y_new) = cyrus_beck_clip($x0, $y0, $x1, $y1);
    if ($accept) {
        printf "Accepted, clipped to (%.1f, %.1f) to (%.1f, %.1f)\n", $p0x_new, $p0y_new, $p1x_new, $p1y_new;
    } else {
        print "Rejected\n";
    }
}
