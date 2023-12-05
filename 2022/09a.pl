#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

# 4964 -> low

use Data::Dumper;
use List::Util qw/max all/;

my @h = (0, 0);
my @t = (0, 0);

my %vis = qw/0;0 1/;

my %dirs = ( L => [-1, 0], R => [1, 0], U => [0, -1], D => [0, 1] );

sub touching { all { abs( $h[$_] - $t[$_] ) <= 1 } (0, 1) }
sub vecadd { ($_[0][0] + $_[1][0], $_[0][1] + $_[1][1]) }
sub vecmul { ($_[0] * $_[1][0], $_[0] * $_[1][1]) }

while ( <> ) {
    /^([LRUD]) (\d+)$/ or die "$_";
    my @dv = $dirs{$1}->@*;
    for ( 1 .. $2 ) {
        @h = vecadd( \@h, \@dv );
        next if touching;
        @t = vecadd( \@t, \@dv );
        die if not touching;
        my ($dx, $dy) = ($t[0] - $h[0], $t[1] - $h[1]);
        next if $dx * $dy == 0; # already ok when one of the coords is 0
        die unless abs( $dx * $dy ) == 1;
        $t[0] = $h[0] if $dv[1];
        $t[1] = $h[1] if $dv[0];
        die unless touching and $t[0] == $h[0] || $t[1] == $h[1];
        next;

        @dv = @dv[1,0];
        @t = vecadd( \@t, \@dv );
        die if touching and $t[0] != $h[0] && $t[1] != $h[1];
        next if touching;
        @dv = vecmul( -2, \@dv );
        @t = vecadd( \@t, \@dv );
    }
    continue {
        $vis{"$t[0];$t[1]"}++;
    }
}

say scalar keys %vis;
