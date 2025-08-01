use strict;
use warnings;

package LFUCache {
    sub new {
        my ($class, $capacity) = @_;
        return bless { capacity => $capacity, cache => {}, freq => {}, order => [] }, $class;
    }

    sub get {
        my ($self, $key) = @_;
        if (exists $self->{cache}->{$key}) {
            $self->{freq}->{$key}++;
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
            $self->{cache}->{$key} = $value;
            $self->{freq}->{$key}++;
        } else {
            if (keys %{$self->{cache}} == $self->{capacity}) {
                my $min_freq = (sort { $self->{freq}->{$a} <=> $self->{freq}->{$b} } @{$self->{order}})[0];
                delete $self->{cache}->{$min_freq};
                delete $self->{freq}->{$min_freq};
                @{$self->{order}} = grep { $_ != $min_freq } @{$self->{order}};
            }
            $self->{cache}->{$key} = $value;
            $self->{freq}->{$key} = 1;
            push @{$self->{order}}, $key;
        }
        print "Cache after put($key, $value): [";
        my $i = 0;
        for my $k (@{$self->{order}}) {
            print "($k, $self->{cache}->{$k})";
            print ", " if $i++ < $#{$self->{order}};
        }
        print "]\n";
    }
}

my $cache = LFUCache->new(3);
$cache->put(1, 10);
$cache->put(2, 20);
$cache->put(3, 30);
$cache->put(4, 40);
print "Get(2) = ", $cache->get(2), "\n";
$cache->put(5, 50);
