use strict;
use warnings;

sub remove_duplicates {
    my ($arr) = @_;
    my %seen;
    @$arr = sort { $a <=> $b } grep { !$seen{$_}++ } @$arr;
}

sub set_difference {
    my ($a, $b) = @_;
    my %b_set = map { $_ => 1 } @$b;
    return [grep { !$b_set{$_} } @$a];
}

sub external_bfs {
    my ($adj, $start, $M, $B) = @_;
    my $n = @$adj;
    my @visited = (0) x $n;
    my @level = (0) x $M;
    my @prev_level = (0) x $M;
    my @prev_prev_level = (0) x $M;
    my $level_size = 0;
    my $prev_level_size = 0;
    my $prev_prev_level_size = 0;
    my $t = 0;

    # Initialize L(0) = {start}
    $level[0] = $start;
    $level_size = 1;
    $visited[$start] = 1;
    print "Level $t: $start\n";

    while ($level_size > 0) {
        # Step 1: Compute A(t) = neighbors of L(t-1)
        my @neighbors;
        for my $i (0 .. $level_size - 1) {
            my $v = $level[$i];
            push @neighbors, @{$adj->[$v]};
        }

        # Step 2: Compute A'(t) by removing duplicates
        remove_duplicates(\@neighbors);

        # Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
        my @temp = (@prev_level[0 .. $prev_level_size - 1], @prev_prev_level[0 .. $prev_prev_level_size - 1]);
        remove_duplicates(\@temp);
        my $new_level = set_difference(\@neighbors, \@temp);

        # Update visited
        $visited[$_] = 1 for @$new_level;

        # Print current level
        if (@$new_level) {
            print "Level ", $t + 1, ": ", join(" ", @$new_level), "\n";
        }

        # Update levels
        $prev_prev_level_size = $prev_level_size;
        @prev_prev_level = @prev_level;
        $prev_level_size = $level_size;
        @prev_level = @level;
        $level_size = @$new_level;
        @level[0 .. $level_size - 1] = @$new_level;
        $t++;
    }
}

my $n = 10;
my @adj = ([]) x $n;
my @edges = ([0,1], [0,3], [0,9], [1,0], [1,2], [1,4], [2,1], [2,3],
             [3,0], [3,2], [3,4], [4,1], [4,3], [4,5], [5,4], [5,6],
             [5,8], [6,5], [6,7], [7,6], [7,8], [8,5], [8,7], [8,9],
             [9,0], [9,8]);
for my $e (@edges) {
    push @{$adj[$e->[0]]}, $e->[1];
}

print "Adjacency lists:\n";
for my $i (0 .. $n - 1) {
    print "$i: @{$adj[$i]}\n";
}

external_bfs(\@adj, 0, 5, 2);
