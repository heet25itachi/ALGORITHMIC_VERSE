use strict;
use warnings;
use Math::Trig;

package Node;
sub new {
    my ($class, $value, $op_type, $children) = @_;
    return bless { value => $value, grad => 0.0, op_type => $op_type, children => $children || [] }, $class;
}

sub backward {
    my ($f_node) = @_;
    my @stack = ($f_node);
    $f_node->{grad} = 1.0;

    while (@stack) {
        my $curr = pop @stack;
        if ($curr->{op_type} eq 'mul') {
            my ($left, $right) = @{$curr->{children}};
            $left->{grad} += $curr->{grad} * $right->{value};
            $right->{grad} += $curr->{grad} * $left->{value};
            push @stack, $left, $right;
        } elsif ($curr->{op_type} eq 'sin') {
            my $child = $curr->{children}[0];
            $child->{grad} += $curr->{grad} * cos($child->{value});
            push @stack, $child;
        }
    }
}

package main;
my $x = 3.14;
my $x_node = Node->new($x, 'var');
my $x2_node = Node->new($x * $x, 'mul', [$x_node, $x_node]);
my $sin_x_node = Node->new(sin($x), 'sin', [$x_node]);
my $f_node = Node->new($x2_node->{value} * $sin_x_node->{value}, 'mul', [$x2_node, $sin_x_node]);

Node::backward($f_node);
printf "f(x) = %.2f, f'(x) = %.2f\n", $f_node->{value}, $x_node->{grad};
