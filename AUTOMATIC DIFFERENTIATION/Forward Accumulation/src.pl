use strict;
use warnings;
use Math::Trig;

package Dual;
sub new {
    my ($class, $v, $d) = @_;
    return bless { value => $v, deriv => $d }, $class;
}

sub add {
    my ($self, $b) = @_;
    return Dual->new($self->{value} + $b->{value}, $self->{deriv} + $b->{deriv});
}

sub mul {
    my ($self, $b) = @_;
    return Dual->new($self->{value} * $b->{value}, $self->{deriv} * $b->{value} + $self->{value} * $b->{deriv});
}

sub sin_dual {
    my $self = shift;
    return Dual->new(sin($self->{value}), cos($self->{value}) * $self->{deriv});
}

my $x = Dual->new(3.14, 1.0);
my $x2 = $x->mul($x);
my $sx = $x->sin_dual;
my $f = $x2->mul($sx);
printf "f(x) = %.2f, f'(x) = %.2f\n", $f->{value}, $f->{deriv};
