#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my @dirs = ( [1,0,0], [0,1,0], [0,0,1] );
push @dirs, map [ map -$_, @$_ ], @dirs;

my @in = map { chomp; [ split ',' ] } <>;
print Dumper \@in;

my @space;
my $res_a = 0;
for my $c ( @in ) {
    my ($x, $y, $z) = @$c;
    $res_a += 6;
    $space[$x][$y][$z] = 1;
    for my $d ( @dirs ) {
        next if $x + $d->[0] < 0
                || $y + $d->[1] < 0
                || $z + $d->[2] < 0;
        $res_a -= 2 if $space[$x + $d->[0]][$y + $d->[1]][$z + $d->[2]];
    }
}

# 3466 too low
say $res_a;

