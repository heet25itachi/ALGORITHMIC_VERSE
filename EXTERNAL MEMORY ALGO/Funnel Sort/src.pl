use strict;
use warnings;

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

sub k_merger {
    my ($inputs, $input_sizes, $k, $buffer, $buffer_size, $k3) = @_;
    if ($k == 1) {
        my $size = $input_sizes->[0] < $k3 ? $input_sizes->[0] : $k3;
        my @output = @{$inputs->[0]}[0 .. $size - 1];
        splice @{$inputs->[0]}, 0, $size;
        $input_sizes->[0] -= $size;
        return \@output;
    }

    my $sqrt_k = int(sqrt($k));
    my @input_mergers = map { $inputs->[$_ * $sqrt_k] } 0 .. $sqrt_k - 1;
    my @input_merger_sizes = map { $input_sizes->[$_ * $sqrt_k] } 0 .. $sqrt_k - 1;
    my @sub_buffers = map { [ (0) x $buffer_size ] } 0 .. $sqrt_k - 1;
    my @sub_buffer_sizes = (0) x $sqrt_k;

    my $k32 = int($k ** 1.5);
    for my $i (0 .. $sqrt_k - 1) {
        if ($sub_buffer_sizes[$i] < $k32) {
            my $temp_output = k_merger(\@input_mergers, \@input_merger_sizes, $sqrt_k, $sub_buffers[$i], $buffer_size, $k32);
            @{$sub_buffers[$i]}[0 .. @$temp_output - 1] = @$temp_output;
            $sub_buffer_sizes[$i] = @$temp_output;
        }
    }

    my @output_merger_inputs = @sub_buffers;
    my @output_merger_sizes = @sub_buffer_sizes;
    my $output = k_merger(\@output_merger_inputs, \@output_merger_sizes, $sqrt_k, $buffer, $buffer_size, $k3);

    for my $i (0 .. $sqrt_k - 1) {
        splice @{$sub_buffers[$i]}, 0, @$output;
        $sub_buffer_sizes[$i] -= @$output;
    }

    return $output;
}

sub funnelsort {
    my ($arr, $z, $l) = @_;
    my $n = @$arr;
    if ($n <= $z) {
        quicksort($arr, 0, $n - 1);
        return;
    }

    my $k = int(0.5 + ($n ** (1/3)));
    my $sub_size = int(0.5 + ($n / $k));
    my @subarrays;
    my @subarray_sizes;

    print "Sorted subarrays:\n";
    for my $i (0 .. $k - 1) {
        my $size = $i == $k - 1 ? $n - $i * $sub_size : $sub_size;
        my @subarray = @$arr[$i * $sub_size .. $i * $sub_size + $size - 1];
        quicksort(\@subarray, 0, $size - 1);
        push @subarrays, \@subarray;
        push @subarray_sizes, $size;
        print "Subarray $i: @subarray\n";
    }

    my $buffer_size = 2 * int($k ** 1.5);
    my @buffer = (0) x $buffer_size;
    my $output = k_merger(\@subarrays, \@subarray_sizes, $k, \@buffer, $buffer_size, $n);

    @$arr = @$output;
}

my @arr = (64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13);
my $z = 8;
my $l = 2;

print "Initial array: @arr\n";

funnelsort(\@arr, $z, $l);

print "Sorted array: @arr\n";
