#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max sum/;

# Parsing
my @grid;
my $y = 1;
my @start = (undef, 1);
my @size = (0, 0);
my $area;
while ( <> ) {
    last if /^$/;
    my $x;
    for my $c ( /./g ) {
        ++$x;
        $size[0] = max $size[0], $x + 1;
        next if $c eq ' ';
        $grid[$y][$x] = $c;
        ++$area;
        $start[0] = $x if !defined $start[0] && $c eq '.';
    }
    ++$y;
}
$size[1] = $y;
my $l = int( sqrt( $area / 6 ) );

my @path = <> =~ /\d+|[LR]/g;

# Mark vertices with "abstract" coordinates {-1,1}^3 and match each pair of
# "mapped" edges sharing one "abstract" edge
my %visited;
my %edges;
mark( @start, [ -1, -1, -1 ], [ 1, -1, -1 ], [ -1, 1, -1] );

sub opp( $v ) { [ map -$v->[$_], 0..2 ] }
sub mark( $x, $y, $tl, $tr, $bl ) {
    return unless 0 <= $x < $size[0];
    return unless 0 <= $y < $size[1];
    return unless $grid[$y][$x];
    return if $visited{"$x $y"}++;

    my $br = [ map { $tl->[$_] * $tr->[$_] * $bl->[$_] } 0..2 ];

    my $tl_mapped = [ $x,          $y ];
    my $tr_mapped = [ $x + $l - 1, $y ];
    my $bl_mapped = [ $x,          $y + $l - 1 ];
    my $br_mapped = [ $x + $l - 1, $y + $l - 1 ];

    sub edge( $abst1, $abst2, $mapped1, $mapped2 ) {
        push $edges{"@$abst1;@$abst2"}->@*, [ $mapped1, $mapped2 ];
        push $edges{"@$abst2;@$abst1"}->@*, [ $mapped2, $mapped1 ];
    }
    edge( $tl, $tr, $tl_mapped, $tr_mapped );
    edge( $tr, $br, $tr_mapped, $br_mapped );
    edge( $br, $bl, $br_mapped, $bl_mapped );
    edge( $bl, $tl, $bl_mapped, $tl_mapped );

    mark( $x - $l, $y, opp($br),     $tl , opp($tr) );
    mark( $x + $l, $y,     $tr , opp($bl),     $br  );
    mark( $x, $y - $l, opp($br), opp($bl),     $tl  );
    mark( $x, $y + $l,     $bl ,     $br , opp($tr) );
}

my %mapped_edges;
for ( values %edges ) {
    die unless @$_ == 2;
    my @es = map [ $_->[0]->@*, $_->[1]->@* ], @$_;
    $mapped_edges{"@{$es[0]}"} = $_->[1];
    $mapped_edges{"@{$es[1]}"} = $_->[0];
}

sub rot_l(@dir) { ( $dir[1], -$dir[0] ) }
sub rot_r(@dir) { ( -$dir[1], $dir[0] ) }

for my $part_b ( 0, 1 ) {
    my @pos = @start;
    my @dir = (1, 0);
    foreach ( @path ) {
        @dir = rot_l( @dir ) if /L/;
        @dir = rot_r( @dir ) if /R/;
        /\d+/ or next;
        for ( 1 .. $_ ) {
            my @next = map $pos[$_] + $dir[$_], 0, 1;
            if ( $part_b ) {
                unless ( $grid[$next[1]][$next[0]] ) {
                    # Part B: cube wrap
                    # Where are we on which edge
                    my @normal = map abs, rot_l( @dir );
                    my @p1 = map { $pos[$_] - $normal[$_] * ( $pos[$_] % $l - 1) } 0, 1;
                    my @p2 = map { $p1[$_] + $normal[$_] * ( $l - 1 ) } 0, 1;
                    my $d = sum map { abs( $pos[$_] - $p1[$_] ) } 0, 1;
                    # Corresponding edge (and the point on it)
                    my @e = $mapped_edges{"@p1 @p2"}->@*;
                    my @e_dir = map { $e[1][$_] <=> $e[0][$_] } 0, 1;
                    @next = map { $e[0][$_] + $d * $e_dir[$_] } 0, 1;
                    unless ( $grid[$next[1]][$next[0]] eq '#' ) {
                        # Inward direction
                        my @e_normal = map abs, rot_l( @e_dir );
                        @dir = map { -$e_normal[$_] * ( ( $next[$_] - 1 ) % $l <=> $l / 2 ) } 0, 1;
                    }
                }
            }
            else {
                # Part A: simple wrap
                until ( $grid[$next[1]][$next[0]] ) {
                    @next = map { ( $next[$_] + $dir[$_] ) % $size[$_] } 0, 1;
                }
            }
            last if $grid[$next[1]][$next[0]] eq '#';
            @pos = @next;
        }
    }

    my $dir_n = 0;
    until ( $dir[0] == 1 && $dir[1] == 0 ) {
        ++$dir_n;
        @dir = ($dir[1], -$dir[0]);
    }
    say $pos[1] * 1000 + $pos[0] * 4 + $dir_n;
}
