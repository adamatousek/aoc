#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my @s = map { chomp; [ /-?\d+/g ] } <>;
my $scan_y = $s[0][0] > 100 ? 2_000_000 : 10;
my %bad_beacons;

my @lines;
for my $s ( @s ) {
    $bad_beacons{$s->[2]} = 1 if $s->[3] == $scan_y;
    my $r = dist( @$s );
    my $dy = dist( $s->@[0,1,0], $scan_y );
    next unless $dy <= $r;
    my $d = $r - $dy;
    push @lines, [ $s->[0] - $d, $s->[0] + $d ];
}

@lines = sort { $b->[0] <=> $a->[0] || $a->[1] <=> $b->[1] } @lines;

my $res_a = 0;
while ( @lines ) {
    my $l = pop @lines;
    my $r = pop @lines;
    if ( $r and $l->[1] + 1 >= $r->[0] ) {
        # merge
        push @lines, [ $l->[0], max( $r->[1], $l->[1] ) ];
    }
    else {
        $res_a += 1 + $l->[1] - $l->[0];
        say "*** merged line: $l->[0] -- $l->[1]";
        if ( my $bad = grep { $l->[0] <= $_ <= $l->[1] } keys %bad_beacons ) {
            say "***** had $bad beacons on it";
            $res_a -= $bad;
        }
        push @lines, $r if $r;
    }
}

say $res_a;


sub dist($x0, $y0, $x1, $y1) { abs( $x1 - $x0 ) + abs( $y1 - $y0 ) }
