#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max min/;

my @s = map { chomp; [ /-?\d+/g ] } <>;
my $scan_y = $s[0][0] > 100 ? 2_000_000 : 10;

my %beacons;
my @rows;
for my $s ( @s ) {
    $beacons{"$s->[2];$s->[3]"} = 1;
    my $r = dist( @$s );
    for my $y ( max( 0, $s->[1] - $r) .. min( $s->[1] + $r, 2 * $scan_y ) ) {
        my $d = $r - abs( $y - $s->[1] );
        push $rows[$y]->@*, [ $s->[0] - $d, $s->[0] + $d ];
    }
}

say "*** Completed scan, now merging.";

while ( my ($y, $row) = each @rows ) {
    $row = [ sort { $b->[0] <=> $a->[0] || $a->[1] <=> $b->[1] } @$row ];

    my @merged;
    while ( @$row ) {
        my $l = pop @$row;
        my $r = pop @$row;
        if ( $r and $l->[1] + 1 >= $r->[0] ) {
            # merge
            push @$row, [ $l->[0], max( $r->[1], $l->[1] ) ];
        }
        else {
            push @$row, $r if $r;
            push @merged, $l;
        }
    }

    if ( $y == $scan_y ) {
        my $res_a;
        for my $l ( @merged ) {
            $res_a += 1 + $l->[1] - $l->[0]
                    - grep { my @b = split /;/; $b[1] == $y && $l->[0] <= $b[0] <= $l->[1] } keys %beacons;
        }
        say "part 1: $res_a";
    }

    if ( @merged > 0 and @merged > 1 || $merged[0][0] > 0 || $merged[0][1] < 2 * $scan_y ) {
        my $x = $merged[0][0] > 0 ? 0 : $merged[0][1] + 1;
        say 'part 2: ', $x * 4_000_000 + $y;
    }
}

sub dist($x0, $y0, $x1, $y1) { abs( $x1 - $x0 ) + abs( $y1 - $y0 ) }
