use strict;
use warnings;

sub quicksort {
    my ($arr, $left, $right) = @_;
    return if $left >= $right;
    my $pivot = $arr->[$right];
    my ($i, $j) = ($left, $right);
    while ($i < $j) {
        $i++ while $i < $j && $arr->[$i] <= $pivot;
        $j-- while $i < $j && $arr->[$j] > $pivot;
        ($arr->[$i], $arr->[$j]) = ($arr->[$j], $arr->[$i]) if $i < $j;
    }
    ($arr->[$i], $arr->[$right]) = ($arr->[$right], $arr->[$i]);
    quicksort($arr, $left, $i - 1);
    quicksort($arr, $i + 1, $right);
}

sub merge_runs {
    my ($run1, $run2, $B) = @_;
    my @output;
    my ($i, $j, $k) = (0, 0, 0);
    while ($i < @$run1 && $j < @$run2) {
        for (1..$B) {
            if ($i < @$run1 && ($j >= @$run2 || $run1->[$i] <= $run2->[$j])) {
                $output[$k++] = $run1->[$i++];
            } elsif ($j < @$run2) {
                $output[$k++] = $run2->[$j++];
            }
        }
    }
    push @output, @$run1[$i..$#$run1];
    push @output, @$run2[$j..$#$run2];
    return \@output;
}

sub external_merge_sort {
    my ($arr, $M, $B) = @_;
    my $n = @$arr;
    if ($n <= $M) {
        quicksort($arr, 0, $n - 1);
        return;
    }

    # Step 1: Divide and sort runs
    my @runs;
    for (my $i = 0; $i < $n; $i += $M) {
        my $size = $i + $M > $n ? $n - $i : $M;
        my @run = @$arr[$i .. $i + $size - 1];
        quicksort(\@run, 0, $size - 1);
        push @runs, [@run];
    }

    # Print sorted runs
    print "Sorted runs:\n";
    for my $i (0..$#runs) {
        print "Run $i: @{$runs[$i]}\n";
    }

    # Step 2: Merge runs (2-way merge for M/B = 2)
    my $output = merge_runs($runs[0], $runs[1], $B);
    @$arr = @$output;
}

my @arr = (64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13);
my $M = 8;
my $B = 4;
print "Initial array: @arr\n";
external_merge_sort(\@arr, $M, $B);
print "Sorted array: @arr\n";
