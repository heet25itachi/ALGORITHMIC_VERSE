use strict;
use warnings;

my $R = 65536;

sub birthday_attack {
    my %table;
    my $trials = 0;
    while (1) {
        my $x = int(rand(2**64));
        my $h = $x % $R;
        $trials++;
        if (exists $table{$h} && $table{$h} != $x) {
            print "Collision found after $trials trials: x_i = $table{$h}, x_j = $x, hash = $h\n";
            last;
        }
        $table{$h} = $x;
    }
}

birthday_attack();
