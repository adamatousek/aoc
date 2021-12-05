#!/usr/bin/perl

use warnings;
use strict;

use List::Util qw/max/;

my $points_a = {};
my $points_b = {};

while (<>) {
    my ( $x1, $y1, $x2, $y2 ) = $_ =~ /^(\d+),(\d+) -> (\d+),(\d+)$/ or next;

    my $dx = $x2 <=> $x1;
    my $dy = $y2 <=> $y1;
    my $l = max( abs( $x1 - $x2 ), abs( $y1 - $y2 ) );

    for ( 0 .. $l ) {
        $points_b->{"$x1,$y1"}++;
        $points_a->{"$x1,$y1"}++ if abs($dx) != abs($dy);
        $x1 += $dx;
        $y1 += $dy;
    }
}

for my $part ($points_a, $points_b) {
    my $cross_count = 0;
    foreach ( values %$part ) {
        ++$cross_count if $_ > 1;
    }
    print "$cross_count\n";
}
