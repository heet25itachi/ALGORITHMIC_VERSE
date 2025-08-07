use strict;
use warnings;

sub approximate_median {
    my ($arr) = @_;
    return 0 unless @$arr;
    my $mid = int(@$arr / 2);
    my @values = ($arr->[0], $arr->[$mid], $arr->[-1]);
    return (sort { $a <=> $b } @values)[1];
}

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

sub copy_elems {
    my ($arr, $next, $bnum, $buckets, $subarray_size, $bucket_idx, $sqrt_n) = @_;
    while ($next->[$bucket_idx] < $subarray_size) {
        if ($bnum->[$bucket_idx] >= @$buckets) {
            push @$buckets, { elements => [], pivot => 1e9 };
        }
        if ($arr->[$next->[$bucket_idx]] <= $buckets->[$bnum->[$bucket_idx]]->{pivot}) {
            if (@{$buckets->[$bnum->[$bucket_idx]]->{elements}} >= 2 * $sqrt_n) {
                my $median = approximate_median($buckets->[$bnum->[$bucket_idx]]->{elements});
                push @$buckets, { elements => [], pivot => $buckets->[$bnum->[$bucket_idx]]->{pivot} };
                $buckets->[$bnum->[$bucket_idx]]->{pivot} = $median;
                my @new_elements;
                my @new_bucket_elements;
                for my $x (@{$buckets->[$bnum->[$bucket_idx]]->{elements}}) {
                    if ($x <= $median) {
                        push @new_elements, $x;
                    } else {
                        push @new_bucket_elements, $x;
                    }
                }
                $buckets->[$bnum->[$bucket_idx]]->{elements} = [@new_elements];
                $buckets->[-1]->{elements} = [@new_bucket_elements];
                for my $i (0..$#$bnum) {
                    $bnum->[$i]++ if $bnum->[$i] > $bnum->[$bucket_idx];
                }
            }
            push @{$buckets->[$bnum->[$bucket_idx]]->{elements}}, $arr->[$next->[$bucket_idx]];
            $next->[$bucket_idx]++;
        } else {
            $bnum->[$bucket_idx]++;
        }
    }
}

sub distribute {
    my ($arr, $next, $bnum, $i, $j, $m, $buckets, $sqrt_n) = @_;
    if ($m == 1) {
        copy_elems([@$arr[$i * $sqrt_n .. ($i + 1) * $sqrt_n - 1]], $next, $bnum, $buckets, $sqrt_n, $i, $sqrt_n);
    } else {
        distribute($arr, $next, $bnum, $i, $j, $m / 2, $buckets, $sqrt_n);
        distribute($arr, $next, $bnum, $i + $m / 2, $j, $m / 2, $buckets, $sqrt_n);
        distribute($arr, $next, $bnum, $i, $j + $m / 2, $m / 2, $buckets, $sqrt_n);
        distribute($arr, $next, $bnum, $i + $m / 2, $j + $m / 2, $m / 2, $buckets, $sqrt_n);
    }
}

sub cache_oblivious_sort {
    my ($arr) = @_;
    my $n = @$arr;
    return if $n <= 1;
    my $sqrt_n = int(sqrt($n));
    return if $sqrt_n * $sqrt_n != $n;

    # Step 1: Partition and sort subarrays
    for my $i (0 .. $sqrt_n - 1) {
        quicksort($arr, $i * $sqrt_n, $i * $sqrt_n + $sqrt_n - 1);
    }

    # Step 2: Distribute
    my @next = (0) x $sqrt_n;
    my @bnum = (0) x $sqrt_n;
    my @buckets = ({ elements => [], pivot => 1e9 });
    distribute($arr, \@next, \@bnum, 0, 0, $sqrt_n, \@buckets, $sqrt_n);

    # Step 3: Sort buckets
    for my $bucket (@buckets) {
        @$bucket->{elements} = sort { $a <=> $b } @{$bucket->{elements}};
    }

    # Step 4: Concatenate
    my $k = 0;
    for my $bucket (@buckets) {
        for my $x (@{$bucket->{elements}}) {
            $arr->[$k++] = $x;
        }
    }
}

my @arr = (64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13);
print "Initial array: @arr\n";
cache_oblivious_sort(\@arr);
print "Sorted array: @arr\n";
