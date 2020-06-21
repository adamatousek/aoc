#!/usr/bin/perl
use v5.16;
use strict;
use warnings;

my $W = 25;
my $H = 6;
my $Lsize = $W * $H;

my @img = <> =~ /(\d)/g;
my @render = (2) x $Lsize;

my $min_zeroes = $Lsize;
my $min_prod = 0;

my $i = 0;
while ( $i < @img ) {
    my @count = (0, 0, 0);
    for ( my $j = 0; $j < $Lsize; $j++, $i++ ) {
        my $p = $img[ $i ];
        $count[ $p ] += 1;
        $render[$j] = $p if ( $render[$j] == 2 );
    }
    if ( $count[0] < $min_zeroes ) {
        $min_zeroes = $count[0];
        $min_prod = $count[1] * $count[2];
    }
}

say "Min zeroes: ", $min_zeroes;
say "Product: ", $min_prod;
say "Image:";

for (my $y = 0; $y < $H; $y++) {
    for (my $x = 0; $x < $W; $x++) {
        my $p = $render[$y*$W + $x];
        print (($p == 0) ? " " : (($p == 1) ? "X" : "-"));
    }
    print "\n"
}
