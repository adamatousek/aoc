#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

use threads;
use Thread::Queue;

my @RESOURCES = qw/ore clay obsidian geode/;
my %RESOURCES = ( map { $RESOURCES[$_] => $_ }  0 .. $#RESOURCES );


my @bps;

while ( <> ) {
    /Blueprint (\d+):/ or next;
    my $bp = [];
    push @bps, $bp;
    push @$bp, [0,0,0] for 1 .. 4;
    while ( /Each (\w+) robot costs ([^.]+)./g ) {
        my $out = $RESOURCES{$1} // die "$1";
        my $deps = $2;
        while ( $deps =~ /(\d+) (\w+)/g ) {
            my $dep = $RESOURCES{$2} // die "$2";
            $bp->[$out][$dep] = 0+$1;
        }
    }
}

print Dumper \@bps;

my $q = Thread::Queue->new;
my $results = Thread::Queue->new;

my @threads;
my $max_time = 24;
for ( 1 .. 4 ) {
    my $t = threads->create( sub {
        while ( defined( my $i = $q->dequeue ) ) {
            say "working on $i";
            $results->enqueue( [$i, solve( [], $bps[$i], $max_time, [1, 0, 0], [0, 0, 0] ) ] );
        }
    } );
    push @threads, $t;
}

my $res_a = 0;
for my $i ( 0 .. $#bps ) {
    $q->enqueue( $i );
}
$q->end;

$_->join for @threads;
$results->end;

while ( defined( my $r = $results->dequeue ) ) {
    say "$r->[0] -> $r->[1]";
    $res_a += $r->[1] * ($r->[0] + 1);
}

say $res_a;

exit;

sub solve($memo, $bp, $t, $bots, $res) {
#say "*** $t: bots @$bots, resources @$res";
    return 0 if $t <= 1;
    my $m = \$memo->[$t]{join $;, @$bots}{join $;, @$res};
    return $$m if defined $$m;
    my $best = 0;
    RECIPE:
    for my $out ( 0 .. $bp->$#* ) {
        my @new_res;
        for ( 0 .. 2 ) {
            $new_res[$_] = $res->[$_] - $bp->[$out][$_];
            next RECIPE if $new_res[$_] < 0;
            $new_res[$_] += $bots->[$_];
        }
        my @new_bots = @$bots;
        $new_bots[$out]++ if $out < 3;
        my $sol = solve( $memo, $bp, $t - 1, \@new_bots, \@new_res );
        $sol += $t - 1 if $out == 3;
        $best = max $best, $sol;
    }
    my @new_res;
    $new_res[$_] = $res->[$_] + $bots->[$_] for 0 .. 2;
    $best = max $best, solve( $memo, $bp, $t - 1, $bots, \@new_res );
    $$m = $best;
    return $best;
}
