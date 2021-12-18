#!/usr/bin/perl

use warnings;
use strict;
use List::Util qw/min max/;

my %paper;
while ( <> ) {
    last if /^$/;
    chomp;
    $paper{$_} = 1;
}

my $count_a;
while ( <> ) {
    my ($axis, $n) = /^fold along ([xy])=(\d+)/ or die;
    my $isx = $axis eq 'x';
    for ( keys %paper ) {
        my ($x, $y) = split /,/;
        my $m = $isx ? $x : $y;
        next if $m < $n;
        $m = $n - ($m - $n);
        delete $paper{"$x,$y"};
        ($x, $y) = ( $isx ? $m : $x, $isx ? $y : $m );
        $paper{"$x,$y"} = 1;
    }
    if ( !$count_a ) {
        $count_a = keys %paper;
        print "$count_a\n\n";
    }
}

my ( $minx, $maxx, $miny, $maxy ) = ( 1000, 0, 1000, 0 );
for ( keys %paper ) {
    $count_a++;
    my ($x, $y) = split /,/;
    $minx = min $minx, $x;
    $maxx = max $maxx, $x;
    $miny = min $miny, $y;
    $maxy = max $maxy, $y;
}

for my $y ( $miny .. $maxy ) {
    for my $x ( $minx .. $maxx ) {
        print $paper{"$x,$y"} ? '#' : ' ';
    }
    print "\n";
}
