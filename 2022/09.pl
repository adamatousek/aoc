#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use List::Util qw/all/;

my %dirs = ( L => [-1, 0], R => [1, 0], U => [0, 1], D => [0, -1] );

sub touching($k1, $k2) { all { abs( $k1->[$_] - $k2->[$_] ) <= 1 } (0, 1) }
sub vecadd { [$_[0][0] + $_[1][0], $_[0][1] + $_[1][1]] }
sub sgn($x) { $x ? $x / abs($x) : 0 }

my @in = <>;

foreach ( 2, 10 ) {
    my @r;
    push @r, [0, 0] for 1 .. $_;
    my %vis = qw/0;0 1/;

    foreach ( @in ) {
        /^([LRUD]) (\d+)$/ or die "$_";
        STEP:
        for ( 1 .. $2 ) {
            my @dv = $dirs{$1}->@*;
            $r[0] = vecadd( $r[0], \@dv );

            for my $i ( 1 .. $#r ) {
                my $h = $i - 1;
                next STEP if touching($r[$h],$r[$i]);
                @dv = map sgn($_), ($r[$h][0] - $r[$i][0], $r[$h][1] - $r[$i][1]);
                $r[$i] = vecadd( $r[$i], \@dv );
                die unless touching($r[$h],$r[$i]);
            }
        }
        continue {
            $vis{"$r[-1][0];$r[-1][1]"}++;
        }
    }
    say scalar keys %vis;
}
