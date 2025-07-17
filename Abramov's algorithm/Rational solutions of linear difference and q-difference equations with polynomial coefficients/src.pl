sub y {
    my ($k, $steps) = @_;
    my $result = 1.0; # y(0) = 1
    for my $i (0..$steps-1) {
        $result *= $i; # Y(k+1) = k * y(k)
    }
    return $result;
}

my $k = 5;
print "Numerical solution for y($k) = ", y($k, $k), "\n";
