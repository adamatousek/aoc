#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my @in = <>;

my $cycle = 1;
my $x = 1;

my $res_a;
my $res_b;

sub step() {
    if ( 20 <= $cycle <= 220 && ( $cycle - 20 ) % 40 == 0 ) {
        say "cycle $cycle: X = $x";
        $res_a += $x * $cycle;
    }
    my $col = ($cycle - 1) % 40;
    if ( $cycle <= 240 ) {
        $res_b .= "\n" if $col == 0;
        $res_b .= ( $x - 1 <= $col <= $x + 1 ) ? '#' : '.';
    }
    ++$cycle;
}

foreach ( @in ) {
    step;
    if ( /(-?\d+)/ ) {
        step;
        say "addx $1";
        $x += $1;
    }
}

say $res_a;
say $res_b;
