#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;
use open qw/:std :utf8/;

use Data::Dumper;
use List::Util qw/max/;

my @map;
my $depth = 0;

while ( <> ) {
    my @last;
    while ( /(\d+),\s*(\d+)/g ) {
        my @c = ($1, $2);
        @last = @c unless defined $last[0];
        $depth = max( $depth, $2 );
        $map[$last[0]][$last[1]] = '1';
        while ( $c[0] != $last[0] || $c[1] != $last[1] ) {
            $last[$_] += sgn( $c[$_] - $last[$_] ) for 0, 1;
            $map[$last[0]][$last[1]] = '1';
        }
    }
}

sub map_wrap($x, $y) { $y >= $depth + 2 ? 1 : $map[$x][$y] }

my $sand = 0;
my $res_a;
GRAIN:
until ( $map[500][0] ) {
    my ($x, $y) = (500, 0);
    FALL:
    while ( 1 ) {
        $res_a = $sand if $y > $depth && !defined $res_a;
        for my $dx ( 0, -1, 1 ) {
            if ( !map_wrap($x + $dx, $y + 1) ) {
                $y++;
                $x += $dx;
                next FALL;
            }
        }
        last FALL;
    }
    $sand++;
    $map[$x][$y] = 2;
}

for my $y ( 0 .. $depth + 2 ) {
    for my $x ( 430 .. 550 ) {
        my $c = map_wrap($x, $y);
        print $c ? ( $c == '1' ? '█' : '.' )
                 : ( $y == 0 && $x == 500 ) ? '*' : ' ';
    }
    print "\n";
}

say $res_a;
say $sand;

sub sgn($x) { $x < 0 ? -1 : $x > 0 ? 1 : 0 }
