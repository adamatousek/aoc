#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my $start;
my $dest;
my @grid;

while ( <> ) {
    chomp;
    my $x = 0;
    my @row;
    foreach my $c ( /./g ) {
        my $y = $. - 1;
        my $cell = { x => $x, y => $y, dist => 'inf' };
        if ( $c eq 'S' ) {
            $start = $cell;
            $cell->{is_start} = 1;
            $c = 'a';
        }
        elsif ( $c eq 'E' ) {
            $dest = $cell;
            $cell->{is_dest} = 1;
            $c = 'z';
        }
        $cell->{h} = ord( $c ) - ord( 'a' );
        ++$x;
        push @row, $cell;
    }
    push @grid, \@row;
}

my @q_current = ( $dest );
my @q_next;

my $res_b;

my $dist = 0;
$dest->{dist} = $dist;

WAVE:
while ( @q_current ) {
    ++$dist;
    for my $c ( @q_current ) {
        die "Has distance $c->{dist}, but @{[$dist - 1]} expected" unless $c->{dist} == $dist - 1;
        for my $n ( neighs( $c ) ) {
            next if $c->{h} > $n->{h} + 1;
            next if $n->{dist} <= $dist;
            $n->{dist} = $dist;
            $res_b = $n if !defined $res_b && $n->{h} == 0;
            last WAVE if $n->{is_start};
            push @q_next, $n;
        }
    }
    @q_current = splice @q_next;
};

say $start->{dist};
say $res_b->{dist};
exit;

foreach ( @grid ) {
    for ( @$_ ) {
        if ( $_->{dist} > 1000 ) {
            print '  oo';
        } else {
            printf '%4d', $_->{dist};
        }
    }
    print "\n";
}

sub neighs($c) {
    my @n;
    foreach ( [0,1], [0,-1], [1,0], [-1,0] ) {
        my $x = $c->{x} + $_->[0];
        my $y = $c->{y} + $_->[1];
        push @n, $grid[ $c->{y} + $_->[1] ][ $x ]
            if 0 <= $y < @grid && 0 <= $x < $grid[0]->@*;
    }
    return @n;
}
