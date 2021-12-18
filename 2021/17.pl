#!/usr/bin/perl

use warnings;
use strict;
use Data::Dumper;
use List::Util qw/min max/;

my ($xfrom, $xto, $yfrom, $yto) = <> =~ /(-?\d+)/g;

my @xs = map { x_min_steps( $_ ) } ( int( sqrt( 2 * $xfrom ) - 1 ) .. ( $xto ) );
my $max_y = 0;
my $count_possible = 0;

for ( @xs ) {
    my $vx = $_->{x};
    #print "* trying vx=$vx\n";
    my $min_steps = $_->{steps};
    my $freefall = $_->{freefall};
    my $vy = $yfrom;
    YPSILON:
    while ( 1 ) {
        #print "* * trying vy=$vy\n";
        my $steps = $min_steps;
        my ($x, $y);
        my $counted = 0;
        do {
            #print "* * * steps=$steps, max_y=", x_after( $vy, $steps ), "\n";
            $x = x_after( $vx, $steps );
            $y = y_after( $vy, $steps );
            my $weightless = x_after( $vy, $steps );
            my $drop = $weightless - $yto;
            if ( $x >= $xfrom && $x <= $xto && $y >= $yfrom && $y <= $yto ) {
                $max_y = max $max_y, $weightless;
                $count_possible++ unless $counted++;
            }
            last YPSILON if $x > $xto && $y > $yto;
            last YPSILON if int( sqrt( 2 * $drop) ) - 1 > 3 * abs( $yfrom - $yto );
            ++$steps;
        } while ( $x <= $xto && $y > $yfrom );
        ++$vy;
    }
}

print "$max_y\n$count_possible\n";

sub x_min_steps {
    my $v = shift;
    my $orig_v = $v;
    my $x = 0;
    my $steps = 0;
    while ( $v && $x < $xto ) {
        $x += $v;
        $v -= 1;
        ++$steps;
        return ({ x => $orig_v, steps => $steps, freefall => $v == 0 })
            if $x >= $xfrom && $x <= $xto;
    }
    return ();
}

sub x_after {
    my ($v, $steps) = @_;
    $steps = min $steps, $v;
    return $v * $steps - (($steps)*($steps - 1)) / 2;
}

sub y_after {
    my ($v, $steps) = @_;
    return $v * $steps - (($steps)*($steps - 1)) / 2;
}
