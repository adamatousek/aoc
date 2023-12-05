#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max min/;

use threads;
use Thread::Queue;

my @RESOURCES = qw/ore clay obsidian geode/;
my %RESOURCES = ( map { $RESOURCES[$_] => $_ }  0 .. $#RESOURCES );


my @bps;
my @max_costs;

while ( <> ) {
    /^Blueprint \d+:/ or next;
    my $bp = [];
    push @bps, $bp;
    push @$bp, [0,0,0] for 1 .. 4;
    while ( /Each (\w+) robot costs ([^.]+)./g ) {
        my $out = $RESOURCES{$1} // die "$1";
        my $deps = $2;
        while ( $deps =~ /(\d+) (\w+)/g ) {
            my $dep = $RESOURCES{$2} // die "$2";
            $bp->[$out][$dep] = 0+$1;
            $max_costs[$#bps][$dep] = max $max_costs[$#bps][$dep] // 0, $1;
        }
    }
}

# Add some unnecessary over-head.
my $q = Thread::Queue->new;
my $results = Thread::Queue->new;

my @threads;
for ( 1 .. min 6, @bps ) {
    my $t = threads->create( sub {
        while ( defined( my $i = $q->dequeue ) ) {
            my @memo;
            my $estimate = 0;
            my $sol_a = solve( \@memo, \$estimate, $bps[$i], $max_costs[$i], 24, [1, 0, 0], [0, 0, 0] );
            my $sol_b = $i < 3 ? solve( \@memo, \$estimate, $bps[$i], $max_costs[$i], 32, [1, 0, 0], [0, 0, 0] ) : 1;
            $results->enqueue([ $i, $sol_a, $sol_b ]);
            say "*** finished $i ($sol_a, $sol_b)";
        }
    } );
    push @threads, $t;
}

for my $i ( 0 .. $#bps ) {
    $q->enqueue( $i );
}
$q->end;

$_->join for @threads;
$results->end;

my $res_a = 0;
my $res_b = 1;
while ( defined( my $r = $results->dequeue ) ) {
    $res_a += ( $r->[0] + 1 ) * $r->[1];
    $res_b *= $r->[2];
}

say $res_a;
say $res_b;

exit;

# $accum is only used in comparison with $estimate to prune clearly sub-optimal paths
sub solve($memo, $estimate, $bp, $max_cost, $time, $bots, $res, $accum=0) {
    # nothing interesting to do
    return 0 if $time <= 1;
    # no way to get above the lower bound
    return 0 if $time * ($time - 1) / 2 + $accum <= $$estimate;

    # recall memoised value
    my $m = \$memo->[$time]{join $;, @$bots}{join $;, @$res};
    return $$m if defined $$m;

    my $best = 0;

    # Branch out depending on which robot will be built next.
    RECIPE:
    for my $out ( 0 .. 3 ) {
        # we already produce more than we are able to spend
        next if $out < 3 && $bots->[$out] >= $max_cost->[$out];

        # when will the robot be ready?
        my $t = 0;
        # waiting for resources
        for my $r ( 0 .. 2 ) {
            my $missing = $bp->[$out][$r] - $res->[$r];
            next if $missing <= 0; # enough of this resource
            next RECIPE if $missing > 0 && !$bots->[$r]; # unable to obtain enough
            $t = max $t, int( ($missing + $bots->[$r] - 1) / $bots->[$r] ); # how long to wait
            next RECIPE if $t + 1 >= $time; # we can't wait that long
        }
        ++$t; # actual robot building

        # how much time and resources will we have left after the robot is built?
        my $rest = $time - $t;
        my @new_res;
        for my $r ( 0 .. 2 ) {
            $new_res[$r] = $res->[$r] + $t * $bots->[$r] - $bp->[$out][$r];
            # reduce state space by capping resources by what we can possibly spend
            $new_res[$r] = min $rest * $max_cost->[$r], $new_res[$r];
        }
        my @new_bots = @$bots;
        $new_bots[$out]++ if $out < 3;

        my $sol = $out == 3 ? $rest : 0;
        $sol += solve( $memo, $estimate, $bp, $max_cost, $rest, \@new_bots, \@new_res, $sol + $accum );
        $best = max $best, $sol;
    }

    # memoise and update lower bound estimate
    $$m = $best;
    $$estimate = max $$estimate, $best + $accum;
    return $best;
}
