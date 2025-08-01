sub binary_search {
    my ($arr, $target) = @_;
    my $left = 0;
    my $right = @$arr - 1;
    while ($left <= $right) {
        my $mid = int(($left + $right) / 2);
        if ($arr->[$mid] == $target) {
            return $mid;
        } elsif ($arr->[$mid] < $target) {
            $left = $mid + 1;
        } else {
            $right = $mid - 1;
        }
    }
    return -1;
}

my @arr = (1, 3, 4, 7, 9);
my $target = 9;
my $result = binary_search(\@arr, $target);
print "Target $target found at index: $result\n";
