#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max min/;

my @s = map { chomp; my @c = /-?\d+/g; [ @c[0,1], dist( @c ) ] } <>;
my $scan_y = $s[0][0] > 100 ? 2_000_000 : 10;

for my $i1 ( 0 .. $#s - 1 ) {
    for my $i2 ( $i1 + 1 .. $#s ) {
        my $c1 = $s[$i1];
        my $c2 = $s[$i2];
        next unless dist($c1->@[0,1], $c2->@[0,1]) < $c1->[2] + $c2->[2] + 1;
        say "$i1 - $i2";
        my ($x1, $y1, $r1) = @$c1;
        my ($x2, $y2, $r2) = @$c2;
        ++$r1;
        ++$r2;
        my $wh = $r1 + $r2;

        my ($w, $h) = ( abs($x1 - $x2), abs($y1 - $y2) );
        $w = $wh - $w if $h > $w;
        $h = $wh - $h if $w > $h;
    }
}

sub dist($x0, $y0, $x1, $y1) { abs( $x1 - $x0 ) + abs( $y1 - $y0 ) }

