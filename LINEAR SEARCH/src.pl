sub linear_search {
    my ($arr, $target) = @_;
    for my $i (0 .. $#$arr) {
        return $i if $arr->[$i] == $target;
    }
    return -1;
}

my @arr = (3, 7, 1, 9, 4);
my $target = 9;
my $result = linear_search(\@arr, $target);
print "Target $target found at index: $result\n";
