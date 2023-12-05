#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my @grid = map {chomp; [split //]} <>
    or exit;

my ($w, $h) = (scalar $grid[0]->@*, scalar @grid);

$w == $h or die;

my %vis;
my $score = 0;
for my $y ( 0 .. $#grid ) {
    find_vis( $y, 0, 0, 1 );
    find_vis( 0, $y, 1, 0 );
    find_vis( $y, $h - 1, 0, -1 );
    find_vis( $w - 1, $y, -1, 0 );
    for my $x ( 0.. $#grid ) {
        $score = max $score, scenic_score( $x, $y );
    }
}

say scalar keys %vis;
say $score;

sub find_vis( $x, $y, $dx, $dy) {
    my $highest = -1;
    while ( 0 <= $x < $w && 0 <= $y < $h ) {
        my $tree_height = $grid[$y][$x];
        if ( $tree_height > $highest ) {
            $vis{"$y;$x"} = 1;
            $highest = $tree_height;
        }
        last if $tree_height == 9;
        $x += $dx;
        $y += $dy;
    }
}

sub scenic_score($x0, $y0) {
    my $score = 1;
    my ($dx, $dy) = (1, 0);
    my $highest = $grid[$y0][$x0];

    for ( 1 .. 4 ) {
        my ($x, $y) = ($x0 + $dx, $y0 + $dy);
        my $dist = 1;
        while ( 0 < $x < $w - 1 && 0 < $y < $h - 1 ) {
            my $tree_height = $grid[$y][$x];
            last if $tree_height >= $highest;
            $dist++;
            $x += $dx;
            $y += $dy;
        }
        $score *= $dist;
        ($dx, $dy) = (-$dy, $dx);
    }
    return $score;
}
