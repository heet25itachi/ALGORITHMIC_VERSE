use strict;
use warnings;
use POSIX qw(floor);

sub jump_search {
    my ($arr, $target) = @_;
    my $size = @$arr;
    my $step = floor(sqrt($size));
    my $prev = 0;
    while ($arr->[$prev + $step - 1] < $target) {
        $prev = $step;
        $step += floor(sqrt($size));
        return -1 if $prev >= $size;
    }
    while ($prev < $size && $arr->[$prev] < $target) {
        $prev++;
    }
    return $prev if $prev < $size && $arr->[$prev] == $target;
    return -1;
}

my @arr = (1, 3, 4, 7, 9);
my $target = 9;
my $result = jump_search(\@arr, $target);
print "Target $target found at index: $result\n";
