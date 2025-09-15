#!/usr/bin/perl
use strict;
use warnings;

package Packet;
sub new {
    my ($class, $size, $priority, $flow_id) = @_;
    return bless { size => $size, priority => $priority, flow_id => $flow_id }, $class;
}

package Queue;
sub new {
    my ($class, $capacity, $total_bandwidth) = @_;
    return bless { items => [], capacity => $capacity, total_bandwidth => $total_bandwidth }, $class;
}

sub enqueue {
    my ($self, $p) = @_;
    if (scalar(@{$self->{items}}) >= $self->{capacity}) {
        return 0;
    }
    push @{$self->{items}}, $p;
    return 1;
}

sub dequeue {
    my ($self) = @_;
    return shift @{$self->{items}};
}

sub size {
    my ($self) = @_;
    return scalar(@{$self->{items}});
}

package PRIQ;
sub new {
    my ($class, $num_queues, $capacity, $bandwidth) = @_;
    my @queues = map { Queue->new($capacity / $num_queues, $bandwidth / $num_queues) } 0..$num_queues-1;
    return bless { queues => \@queues }, $class;
}

sub enqueue {
    my ($self, $p) = @_;
    my $pri = $p->{priority} % scalar(@{$self->{queues}});
    if ($self->{queues}[$pri]->enqueue($p)) {
        print "Packet enqueued, size: $p->{size}, priority: $pri, flow_id: $p->{flow_id}\n";
    } else {
        print "Queue full, packet dropped, size: $p->{size}\n";
    }
}

sub dequeue_all {
    my ($self) = @_;
    for my $pri (reverse 0..$#{$self->{queues}}) {
        while ($self->{queues}[$pri]->size > 0) {
            my $p = $self->{queues}[$pri]->dequeue;
            print "Packet dequeued, size: $p->{size}, priority: $pri, flow_id: $p->{flow_id}\n";
        }
    }
}

package CoDelQueue;
sub new {
    my ($class, $capacity, $bandwidth, $target, $interval) = @_;
    return bless {
        q => Queue->new($capacity, $bandwidth),
        target_delay => $target,
        interval => $interval,
        first_above_time => 0.0,
        drop_next => 'Inf',
        drop_count => 0
    }, $class;
}

sub enqueue {
    my ($self, $p, $current_time) = @_;
    if ($self->{q}->enqueue($p)) {
        print "Packet enqueued, size: $p->{size}, priority: $p->{priority}, flow_id: $p->{flow_id}\n";
        $self->process_queue($current_time);
        return 1;
    }
    print "Queue full, packet dropped, size: $p->{size}\n";
    return 0;
}

sub process_queue {
    my ($self, $current_time) = @_;
    while ($self->{q}->size > 0) {
        my $sojourn_time = $current_time; # Simplified
        if ($sojourn_time < $self->{target_delay} || $self->{q}->size <= 4) {
            my $p = $self->{q}->dequeue;
            print "Packet dequeued, size: $p->{size}, priority: $p->{priority}, flow_id: $p->{flow_id}\n";
            $self->{first_above_time} = 0.0;
            $self->{drop_next} = 'Inf';
            $self->{drop_count} = 0;
        } elsif ($self->{first_above_time} == 0.0) {
            $self->{first_above_time} = $current_time + $self->{interval};
            $self->{drop_next} = $self->{first_above_time};
            my $p = $self->{q}->dequeue;
            print "Packet dequeued, size: $p->{size}, priority: $p->{priority}, flow_id: $p->{flow_id}\n";
        } elsif ($current_time >= $self->{drop_next}) {
            my $p = $self->{q}->dequeue;
            print "Packet dropped, size: $p->{size}, priority: $p->{priority}, flow_id: $p->{flow_id}\n";
            $self->{drop_count}++;
            $self->{drop_next} = $current_time + $self->{interval} / sqrt($self->{drop_count});
        } else {
            my $p = $self->{q}->dequeue;
            print "Packet dequeued, size: $p->{size}, priority: $p->{priority}, flow_id: $p->{flow_id}\n";
            $self->{drop_count} = 0;
        }
    }
}

sub size {
    my ($self) = @_;
    return $self->{q}->size;
}

package CBQ;
sub new {
    my ($class, $num_nodes, $capacity, $bandwidth) = @_;
    my @nodes = map { Queue->new($capacity / $num_nodes, $bandwidth / $num_nodes) } 0..$num_nodes-1;
    return bless { nodes => \@nodes }, $class;
}

sub enqueue {
    my ($self, $p) = @_;
    my $node_idx = $p->{flow_id} % scalar(@{$self->{nodes}});
    if ($self->{nodes}[$node_idx]->enqueue($p)) {
        print "Packet enqueued, size: $p->{size}, priority: $p->{priority}, flow_id: $p->{flow_id}\n";
    } else {
        print "Queue full, packet dropped, size: $p->{size}\n";
    }
}

sub dequeue_all {
    my ($self) = @_;
    for my $node_idx (0..$#{$self->{nodes}}) {
        while ($self->{nodes}[$node_idx]->size > 0) {
            my $p = $self->{nodes}[$node_idx]->dequeue;
            print "Packet dequeued, size: $p->{size}, priority: $p->{priority}, flow_id: $p->{flow_id}\n";
        }
    }
}

package FairQ;
sub new {
    my ($class, $num_flows, $capacity, $bandwidth) = @_;
    my @flow_queues = map { Queue->new($capacity / $num_flows, $bandwidth / $num_flows) } 0..$num_flows-1;
    return bless { flow_queues => \@flow_queues }, $class;
}

sub enqueue {
    my ($self, $p) = @_;
    my $flow = $p->{flow_id} % scalar(@{$self->{flow_queues}});
    if ($self->{flow_queues}[$flow]->enqueue($p)) {
        print "Packet enqueued, size: $p->{size}, priority: $p->{priority}, flow_id: $flow\n";
    } else {
        print "Queue full, packet dropped, size: $p->{size}\n";
    }
}

sub dequeue_all {
    my ($self) = @_;
    for my $flow (0..$#{$self->{flow_queues}}) {
        while ($self->{flow_queues}[$flow]->size > 0) {
            my $p = $self->{flow_queues}[$flow]->dequeue;
            print "Packet dequeued, size: $p->{size}, priority: $p->{priority}, flow_id: $flow\n";
        }
    }
}

package HFSC;
sub new {
    my ($class, $num_nodes, $capacity, $bandwidth) = @_;
    my @nodes = map { Queue->new($capacity / $num_nodes, $bandwidth / $num_nodes) } 0..$num_nodes-1;
    return bless { nodes => \@nodes }, $class;
}

sub enqueue {
    my ($self, $p) = @_;
    my $node_idx = $p->{priority} % scalar(@{$self->{nodes}});
    if ($self->{nodes}[$node_idx]->enqueue($p)) {
        print "Packet enqueued, size: $p->{size}, priority: $p->{priority}, flow_id: $p->{flow_id}\n";
    } else {
        print "Queue full, packet dropped, size: $p->{size}\n";
    }
}

sub dequeue_all {
    my ($self) = @_;
    for my $node_idx (0..$#{$self->{nodes}}) {
        while ($self->{nodes}[$node_idx]->size > 0) {
            my $p = $self->{nodes}[$node_idx]->dequeue;
            print "Packet dequeued, size: $p->{size}, priority: $p->{priority}, flow_id: $p->{flow_id}\n";
        }
    }
}

package main;
srand(42);
my @packets = map { Packet->new(int(rand(100) + 1), int(rand(15) + 1), int(rand(5) + 1)) } 1..200;

sub simulate_altq {
    my ($packets, $capacity, $bandwidth) = @_;
    print "=== ALTQ Schedulers Simulation ===\n";

    print "\n=== PRIQ Scheduler ===\n";
    print "Initial queue: empty\n";
    my $priq = PRIQ->new(16, $capacity, $bandwidth);
    my $dropped = 0;
    foreach my $p (@$packets) {
        $priq->enqueue($p);
    }
    $priq->dequeue_all;
    print "Final queue length: 0\n";
    print "Packets dropped: $dropped\n";
    print "Final queue: empty\n";

    print "\n=== CoDel Scheduler ===\n";
    print "Initial queue: empty\n";
    my $codel = CoDelQueue->new($capacity, $bandwidth, 5.0, 100.0);
    my $current_time = 0.0;
    foreach my $p (@$packets) {
        $codel->enqueue($p, $current_time) || ($dropped++);
        $current_time += 1.0;
    }
    print "Final queue length: ", $codel->size, "\n";
    print "Packets dropped: $dropped\n";
    print "Final queue: empty\n";

    print "\n=== CBQ Scheduler ===\n";
    print "Initial queue: empty\n";
    my $cbq = CBQ->new(4, $capacity, $bandwidth);
    foreach my $p (@$packets) {
        $cbq->enqueue($p);
    }
    $cbq->dequeue_all;
    print "Final queue length: 0\n";
    print "Packets dropped: $dropped\n";
    print "Final queue: empty\n";

    print "\n=== FairQ Scheduler ===\n";
    print "Initial queue: empty\n";
    my $fairq = FairQ->new(5, $capacity, $bandwidth);
    foreach my $p (@$packets) {
        $fairq->enqueue($p);
    }
    $fairq->dequeue_all;
    print "Final queue length: 0\n";
    print "Packets dropped: $dropped\n";
    print "Final queue: empty\n";

    print "\n=== HFSC Scheduler ===\n";
    print "Initial queue: empty\n";
    my $hfsc = HFSC->new(4, $capacity, $bandwidth);
    foreach my $p (@$packets) {
        $hfsc->enqueue($p);
    }
    $hfsc->dequeue_all;
    print "Final queue length: 0\n";
    print "Packets dropped: $dropped\n";
    print "Final queue: empty\n";
}

simulate_altq(\@packets, 100, 1000);
