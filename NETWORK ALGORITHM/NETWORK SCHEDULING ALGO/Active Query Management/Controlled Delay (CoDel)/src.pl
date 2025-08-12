use strict;
use warnings;

# Define Packet and CoDelQueue packages
package Packet;
sub new {
    my ($class, $size, $arrival_time) = @_;
    return bless { size => $size, arrival_time => $arrival_time // 0.0 }, $class;
}

package CoDelQueue;
sub new {
    my ($class, $capacity) = @_;
    return bless { items => [], capacity => $capacity, first_above_time => 0, drop_next => 'inf', drop_count => 0 }, $class;
}

sub enqueue {
    my ($self, $p) = @_;
    if (@{$self->{items}} >= $self->{capacity}) {
        print "Queue full, packet dropped, size: $p->{size}\n";
        return 0;
    }
    push @{$self->{items}}, $p;
    print "Packet enqueued, size: $p->{size}\n";
    return 1;
}

sub dequeue {
    my ($self, $current_time, $target, $interval) = @_;
    while (@{$self->{items}}) {
        my $p = $self->{items}->[0];
        my $sojourn_time = $current_time - $p->{arrival_time};

        if ($sojourn_time < $target || @{$self->{items}} <= 4) {
            $self->{first_above_time} = 0;
            $self->{drop_next} = 'inf';
            shift @{$self->{items}};
            printf "Packet dequeued, size: %d, sojourn time: %.2f\n", $p->{size}, $sojourn_time;
            $self->{drop_count} = 0;
        } elsif ($self->{first_above_time} == 0) {
            $self->{first_above_time} = $current_time + $interval;
            $self->{drop_next} = $self->{first_above_time};
            shift @{$self->{items}};
            printf "Packet dequeued, size: %d, sojourn time: %.2f\n", $p->{size}, $sojourn_time;
        } elsif ($current_time >= $self->{drop_next}) {
            shift @{$self->{items}};
            printf "Packet dropped, size: %d, sojourn time: %.2f\n", $p->{size}, $sojourn_time;
            $self->{drop_count}++;
            $self->{drop_next} = $current_time + $interval / sqrt($self->{drop_count});
        } else {
            shift @{$self->{items}};
            printf "Packet dequeued, size: %d, sojourn time: %.2f\n", $p->{size}, $sojourn_time;
            $self->{drop_count} = 0;
        }
    end
}

package main;

sub simulate_codel {
    my ($packets, $target, $interval, $capacity) = @_;
    my $q = CoDelQueue->new($capacity);
    my $current_time = 0;
    my $dropped = 0;
    print "Initial queue: empty\n";

    foreach my $p (@$packets) {
        my $packet = Packet->new($p->{size}, $current_time);
        if ($q->enqueue($packet)) {
            $q->dequeue($current_time, $target, $interval);
        } else {
            $dropped++;
        }
        $current_time += 1;
    }

    $q->dequeue($current_time, $target, $interval);

    print "Final queue length: ", $q->{items}->@* , "\n";
    print "Packets dropped: $dropped\n";
    print "Final queue: empty\n";
}

# Main
srand(42);
my @packets = map { { size => int(rand(100)) + 1 } } 1..200;
simulate_codel(\@packets, 5, 100, 100);
