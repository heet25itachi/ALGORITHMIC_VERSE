use strict;
use warnings;
use POSIX qw(ceil);

sub quicksort {
    my ($arr, $left, $right) = @_;
    if ($left >= $right) { return; }
    my $pivot = $arr->[$right];
    my $i = $left;
    my $j = $right;
    while ($i < $j) {
        while ($i < $j && $arr->[$i] <= $pivot) { $i++; }
        while ($i < $j && $arr->[$j] > $pivot) { $j--; }
        if ($i < $j) {
            ($arr->[$i], $arr->[$j]) = ($arr->[$j], $arr->[$i]);
        }
    }
    ($arr->[$right], $arr->[$i]) = ($arr->[$i], $arr->[$right]);
    quicksort($arr, $left, $i - 1);
    quicksort($arr, $i + 1, $right);
}

sub pem_select {
    my ($arr, $n, $k) = @_;
    if ($n <= 5) {
        my @temp = sort { $a <=> $b } @$arr[0 .. $n-1];
        return $temp[$k-1];
    }
    return $arr->[$k-1]; # Simplified: use direct access for small arrays
}

sub pem_multipartition {
    my ($arr, $n, $pivots, $d_sqrt, $p) = @_;
    my @counts = (0) x ($p * $d_sqrt);
    for my $i (0 .. $p-1) {
        my $start = $i * ($n / $p);
        my $size = $i == $p-1 ? $n - $start : $n / $p;
        for my $j (0 .. $size-1) {
            my $elem = $arr->[$start + $j];
            my $bucket = 0;
            while ($bucket < $d_sqrt - 1 && $elem > $pivots->[$bucket]) {
                $bucket++;
            }
            $counts[$i * $d_sqrt + $bucket]++;
        }
    }

    my @prefix_sums = (0) x $d_sqrt;
    for my $j (0 .. $d_sqrt-1) {
        for my $i (0 .. $p-1) {
            $prefix_sums[$j] += $counts[$i * $d_sqrt + $j];
        }
    }

    my @buckets = map { [] } 0 .. $d_sqrt-1;
    my @offsets = (0) x $d_sqrt;
    for my $i (0 .. $p-1) {
        my $start = $i * ($n / $p);
        my $size = $i == $p-1 ? $n - $start : $n / $p;
        for my $j (0 .. $size-1) {
            my $elem = $arr->[$start + $j];
            my $bucket = 0;
            while ($bucket < $d_sqrt - 1 && $elem > $pivots->[$bucket]) {
                $bucket++;
            }
            push @{$buckets[$bucket]}, $elem;
            $offsets[$bucket]++;
        }
    }

    return (\@buckets, \@prefix_sums);
}

sub pem_dist_sort {
    my ($arr, $p, $m, $b, $d) = @_;
    my $n = @$arr;
    if ($n <= $m) {
        quicksort($arr, 0, $n-1);
        return;
    }

    my $d_sqrt = int(sqrt($d));
    my $segment_size = $n / $p;

    print "Segments:\n";
    for my $i (0 .. $p-1) {
        my $size = $i == $p-1 ? $n - $i * $segment_size : $segment_size;
        print "Segment $i: ", join(" ", @$arr[$i * $segment_size .. $i * $segment_size + $size - 1]), "\n";
    }

    my @pivots = map { pem_select($arr, $n, ($_ + 1) * $n / $d_sqrt) } 0 .. $d_sqrt-2;
    print "Pivots: ", join(" ", @pivots), "\n";

    my ($buckets, $bucket_sizes) = pem_multipartition($arr, $n, \@pivots, $d_sqrt, $p);

    print "Buckets:\n";
    for my $j (0 .. $d_sqrt-1) {
        print "Bucket $j: ", join(" ", @{$buckets->[$j]}), "\n";
    }

    my @output;
    for my $j (0 .. $d_sqrt-1) {
        my $processors = ceil($bucket_sizes->[$j] / ($n / $p));
        pem_dist_sort($buckets->[$j], $processors, $m, $b, $d);
        push @output, @{$buckets->[$j]};
    }

    @$arr = @output;
}

my @arr = (64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13);
my $p = 4;
my $m = 8;
my $b = 2;
my $d = 4;

print "Initial array: ", join(" ", @arr), "\n";

pem_dist_sort(\@arr, $p, $m, $b, $d);

print "Sorted array: ", join(" ", @arr), "\n";
