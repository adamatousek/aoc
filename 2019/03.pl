#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
use List::Util qw[min max sum];

my @p1 = load_lines();
my @p2 = load_lines();

my $stepsum = 99999999999;
my $steps1 = 0;
my $i = 0;
while ( $i < @p1 - 3 ) {
    my @l1 = @p1[$i..$i+3];
    my $steps2 = 0;
    my $j = 0;
    while ( $j < @p2 - 3 ) {
        my @l2 = @p2[$j..$j+3];
        my @p = intersect( @l1, @l2 );
        if ($p[0] != 0 || $p[1] != 0) {
            my $d1 = distance( @l1[0,1], @p );
            my $d2 = distance( @l2[0,1], @p );
            my $d = $steps1 + $steps2 + $d1 + $d2;
            $stepsum = $d if ( $stepsum > $d );
        }
        $steps2 += distance( @l2 );
        $j += 2;
    }
    $steps1 += distance( @l1 );
    $i += 2;
}

say $stepsum;

sub intersect {
    my ($x11, $y11, $x12, $y12, $x21, $y21, $x22, $y22) = @_;
    if ( $x21 == $x22 && $y11 == $y12 ) {
        return ($x21, $y11) if ( between($x11, $x12, $x21) && between($y21, $y22, $y11) );
    } elsif ( $y21 == $y22 && $x11 == $x12 ) {
        return ($x11, $y21) if ( between($y11, $y12, $y21) && between($x21, $x22, $x11) );
    }
    return (0,0);
}

sub distance {
    my ($x1, $y1, $x2, $y2) = @_;
    return abs($x1 - $x2) + abs($y1 - $y2);
}

sub between {
    my ($a, $b, $x) = @_;
    my $c = min($a, $b);
    my $d = max($a, $b);
    return $c <= $x && $x <= $d;
}

sub load_lines {
    my $in = <>;
    my @foo = $in =~ /([RULD])(\d*)/g;
    my @lastp = (0, 0);
    my @points = (0, 0);
    my $i = 0;
    while ( $i < @foo ) {
        my $d = $foo[$i++];
        my $l = $foo[$i++];
        my ($dx, $dy) = (0, 0);
        if ( $d eq "U" ) { $dy = $l; }
        elsif ( $d eq "D" ) { $dy = -$l; }
        elsif ( $d eq "R" ) { $dx = $l; }
        elsif ( $d eq "L" ) { $dx = -$l; }
        my @p = ($lastp[0] + $dx, $lastp[1] + $dy);
        push( @points, @p );
        @lastp = @p;
    }
    return @points;
}
