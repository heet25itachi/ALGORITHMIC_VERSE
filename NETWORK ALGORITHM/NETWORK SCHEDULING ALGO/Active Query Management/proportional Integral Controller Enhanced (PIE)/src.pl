use strict;
use warnings;

# Define Packet and Queue packages
package Packet;
sub new {
    my ($class, $size) = @_;
    return bless { size => $size, arrival_time => 0.0 }, $class;
}

package Queue;
sub new {
    my ($class, $capacity) = @_;
    return bless { items => [], capacity => $capacity }, $class;
}
sub enqueue {
    my ($self, $p) = @_;
    if (@{$self->{items}} >= $self->{capacity}) {
        return 0;
    }
    push @{$self->{items}}, $p;
    return 1;
}
sub dequeue {
    my $self = shift;
    shift @{$self->{items}};
}
sub peek {
    my $self = shift;
    $self->{items}->[0];
}
sub size {
    my $self = shift;
    scalar @{$self->{items}};
}

package main;

# Simulate PIE algorithm
sub simulate_pie {
    my ($packets, $target, $update_interval, $alpha, $beta, $max_drop_prob, $max_burst, $capacity) = @_;
    my $q = Queue->new($capacity);
    my ($current_time, $last_update, $drop_prob, $prev_delay, $burst_time) = (0, 0, 0, 0, $max_burst);
    my $dropped = 0;
    print "Initial queue: empty\n";
    srand(42);

    foreach my $p (@$packets) {
        $p->{arrival_time} = $current_time;
        my $delay = $q->size == 0 ? 0 : $current_time - $q->peek->{arrival_time};

        if ($current_time - $last_update >= $update_interval) {
            my $error = $delay - $target;
            $drop_prob += $alpha * $error + $beta * ($delay - $prev_delay);
            $drop_prob = ($drop_prob < 0) ? 0 : ($drop_prob > $max_drop_prob ? $max_drop_prob : $drop_prob);
            $prev_delay = $delay;
            $last_update = $current_time;
            if ($delay > $target) {
                $burst_time = 0;
            } elsif ($burst_time < $max_burst) {
                $burst_time += $update_interval;
            }
        }

        my $drop = $burst_time < $max_burst && $delay > $target && rand() < $drop_prob;

        if ($drop) {
            printf "Packet dropped, size: %d, queue delay: %.2f, drop prob: %.4f\n", $p->{size}, $delay, $drop_prob;
            $dropped++;
        } elsif ($q->enqueue($p)) {
            printf "Packet enqueued, size: %d, queue delay: %.2f, drop prob: %.4f\n", $p->{size}, $delay, $drop_prob;
            my $deq_p = $q->dequeue;
            printf "Packet dequeued, size: %d, queue delay: %.2f\n", $deq_p->{size}, $delay;
        } else {
            printf "Queue full, packet dropped, size: %d\n", $p->{size};
            $dropped++;
        }
        $current_time += 1;
    }

    while ($q->size > 0) {
        my $delay = $current_time - $q->peek->{arrival_time};
        if ($current_time - $last_update >= $update_interval) {
            my $error = $delay - $target;
            $drop_prob += $alpha * $error + $beta * ($delay - $prev_delay);
            $drop_prob = ($drop_prob < 0) ? 0 : ($drop_prob > $max_drop_prob ? $max_drop_prob : $drop_prob);
            $prev_delay = $delay;
            $last_update = $current_time;
            if ($delay > $target) {
                $burst_time = 0;
            } elsif ($burst_time < $max_burst) {
                $burst_time += $update_interval;
            }
        }
        my $deq_p = $q->dequeue;
        printf "Packet dequeued, size: %d, queue delay: %.2f\n", $deq_p->{size}, $delay;
        $current_time += 1;
    }

    print "Final queue length: ", $q->size, "\n";
    print "Packets dropped: $dropped\n";
    print "Final queue: empty\n";
}

# Main
my @packets = map { Packet->new(int(rand(100)) + 1) } 1..200;
print "=== PIE ===\n";
simulate_pie(\@packets, 15, 30, 0.125, 1.25, 0.1, 150, 100);
