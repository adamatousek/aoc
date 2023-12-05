#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my @grid;
my $y = 1;
my @start = (undef, 1);
my @size = (0, 0);
while ( <> ) {
    last if /^$/;
    my $x;
    for my $c ( /./g ) {
        ++$x;
        $size[0] = max $size[0], $x + 1;
        next if $c eq ' ';
        $grid[$y][$x] = $c;
        $start[0] = $x unless $start[0] || $c ne '.';
    }
    ++$y;
}
$size[1] = $y;

say "dimensions: @size";
my @path = <> =~ /\d+|[LR]/g;

my @pos = @start;
my @dir = (1, 0);
foreach ( @path ) {
    @dir = ($dir[1], -$dir[0]) if /L/;
    @dir = (-$dir[1], $dir[0]) if /R/;
    /\d+/ or next;
    for ( 1 .. $_ ) {
        my @next = map $pos[$_] + $dir[$_], 0, 1;
        until ( $grid[$next[1]][$next[0]] ) {
            @next = map { ( $next[$_] + $dir[$_] ) % $size[$_] } 0, 1;
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

say "-> @pos";
say "dir: $dir_n";
say $pos[1] * 1000 + $pos[0] * 4 + $dir_n;
