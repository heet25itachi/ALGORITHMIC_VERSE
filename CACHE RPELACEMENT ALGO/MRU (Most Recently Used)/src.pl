use strict;
use warnings;

package MRUCache {
    sub new {
        my ($class, $capacity) = @_;
        return bless { capacity => $capacity, cache => {}, order => [] }, $class;
    }

    sub get {
        my ($self, $key) = @_;
        if (exists $self->{cache}->{$key}) {
            @{$self->{order}} = grep { $_ != $key } @{$self->{order}};
            unshift @{$self->{order}}, $key;
            print "Cache after get($key): [";
            my $i = 0;
            for my $k (@{$self->{order}}) {
                print "($k, $self->{cache}->{$k})";
                print ", " if $i++ < $#{$self->{order}};
            }
            print "]\n";
            return $self->{cache}->{$key};
        }
        return -1;
    }

    sub put {
        my ($self, $key, $value) = @_;
        if (exists $self->{cache}->{$key}) {
            @{$self->{order}} = grep { $_ != $key } @{$self->{order}};
        } elsif (keys %{$self->{cache}} == $self->{capacity}) {
            my $oldest = shift @{$self->{order}};
            delete $self->{cache}->{$oldest};
        }
        $self->{cache}->{$key} = $value;
        unshift @{$self->{order}}, $key;
        print "Cache after put($key, $value): [";
        my $i = 0;
        for my $k (@{$self->{order}}) {
            print "($k, $self->{cache}->{$k})";
            print ", " if $i++ < $#{$self->{order}};
        }
        print "]\n";
    }
}

my $cache = MRUCache->new(3);
$cache->put(1, 10);
$cache->put(2, 20);
$cache->put(3, 30);
$cache->put(4, 40);
print "Get(2) = ", $cache->get(2), "\n";
$cache->put(5, 50);
