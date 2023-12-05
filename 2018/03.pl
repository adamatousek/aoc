#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;

my %claims;
my $claims;
my @overlaps;
while ( <> ) {
    my ($id, $x, $y, $w, $h) = /\d+/g;
    my $overlaps = 0;
    for my $i ($x .. $x + $w - 1) {
        for my $j ($y .. $y + $h - 1) {
            $claims{"$i,$j"} //= [];
            my $p = $claims{"$i,$j"};
            push @$p, $id;
            if ( @$p == 2 ) {
                $claims++;
                $overlaps[$p->[0]] = 1;
            }
            if ( @$p > 1 ) {
                $overlaps[$id] = 1;
            }
        }
    }

}

say $claims;
for my $i (1 .. $#overlaps) {
    say $i unless $overlaps[$i];
}
