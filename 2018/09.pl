#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;
use List::Util qw/sum max/;

my ($players, $last) = <> =~ /\d+/g;

for my $rounds ( $last, 100 * $last ) {
    my $current = { n => 0 };
    $current->{cw} = $current;
    $current->{ccw} = $current;

    my @scores = (0) x $players;

    for my $marble ( 1 .. $rounds ) {
        if ( $marble % 23 == 0 ) {
            $current = $current->{ccw} for 1 .. 7;
            $scores[$marble % $players] += $marble + $current->{n};
            $current->{cw}{ccw} = $current->{ccw};
            $current->{ccw}{cw} = $current->{cw};
            $current = $current->{cw};
        }
        else {
            $current = $current->{cw};
            my $new = { n => $marble, cw => $current->{cw}, ccw => $current };
            $current->{cw}{ccw} = $new;
            $current->{cw} = $new;
            $current = $new;
        }
        # print "$marble:", print_ring( $current ), "\n";
    }

    say max @scores;
}

sub print_ring($m) {
    my $s;
    my $start = $m;
    do {
        $s .= ' ' . $m->{n};
        $m = $m->{cw};
    } while ( $start != $m );
    return $s;
}
