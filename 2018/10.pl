#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;
use List::Util qw/min max/;

my @ps;
while ( <> ) {
    my @d = /-?\d+/g;
    my $p = { pos => [@d[0,1]], vel => [@d[2,3]] };
    push @ps, $p;
}

my @cmax;
my @cmin;
my $steps;

do {
    @cmax = ('-inf', '-inf');
    @cmin = ('inf', 'inf');
    for my $p ( @ps ) {
        for (0, 1) {
            $p->{pos}[$_] += $p->{vel}[$_];
            $cmax[$_] = max $cmax[$_], $p->{pos}[$_];
            $cmin[$_] = min $cmin[$_], $p->{pos}[$_];
        }
    }
    ++$steps;
} while ( $cmax[1] - $cmin[1] > 9 );

@ps = sort { $b->{pos}[1] <=> $a->{pos}[1] || $b->{pos}[0] <=> $a->{pos}[0] } @ps;
for my $y ($cmin[1] .. $cmax[1]) {
    for my $x ($cmin[0] .. $cmax[0]) {
        my $p = $ps[-1] or last;
        my $popped = 0;
        while ( @ps && $ps[-1]{pos}[0] == $x && $ps[-1]{pos}[1] == $y ) {
            pop @ps;
            $popped = 1;
        }
        print( $popped ? '#' : ($x * $y) ? ' ' : '.' );
        last unless @ps;
    }
    print "\n";
}

say $steps;
