#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;
use List::Util qw/min max/;

my @ps;
my @lim = (1000, 1000, 0, 0);
my $safe_area;

while ( <> ) {
    my @coords = /\d+/g;
    push @ps, { id => scalar @ps, coords => \@coords };
    $lim[0] = min $lim[0], $coords[0];
    $lim[1] = min $lim[1], $coords[1];
    $lim[2] = max $lim[2], $coords[0];
    $lim[3] = max $lim[3], $coords[1];
}

my $SAFE_DIST = $lim[2] > 20 ? 10_000 : 32;

for my $y ( $lim[1] - 1 .. $lim[3] + 1 ) {
    for my $x ( $lim[0] - 1 .. $lim[2] + 1 ) {
        my $dmin = 'inf';
        my $pmin;
        my $dsum;
        for my $p ( @ps ) {
            my $d = dist( $x, $y, $p->{coords}->@* );
            $dsum += $d;
            if ( $d < $dmin ) {
                $dmin = $d;
                $pmin = $p;
            }
            elsif ( $d == $dmin ) {
                $pmin = undef;
            }
        }
        if ( defined $pmin ) {
            $pmin->{area}++;
            unless ( $lim[0] <= $x <= $lim[2] && $lim[1] <= $y <= $lim[3] ) {
                $pmin->{inf} = 1;
            }
        }
        $safe_area++ if $dsum < $SAFE_DIST;
    }
}

say max map $_->{area}, grep !$_->{inf}, @ps;
say $safe_area;

sub dist($x1, $y1, $x2, $y2) {
    return abs($x1 - $x2) + abs($y1 - $y2);
}
