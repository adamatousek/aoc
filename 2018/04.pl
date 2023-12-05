#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;

my %gs;
my @in = sort <>;

my $g;
my $fell;
foreach ( @in ) {
    my @date = /\d+/g;
    my $min = $date[4];
    $g = $1 if /Guard #(\d+)/;
    $fell = $min if /falls/;
    if ( /wakes/ ) {
        $gs{$g}{hist} //= [ (0) x 60 ];
        $gs{$g}{total} += $min - $fell;
        my $hist = $gs{$g}{hist};
        $hist->[$_]++ for $fell .. $min;
    }
}

my $gmax;
while ( my ($id, $g) = each %gs ) {
    $g->{id} = $id;
    $gmax = $g if !defined($gmax) || $g->{total} > $gmax->{total};
}
my $mmax = 0;
for my $m ( 0 .. 59 ) {
    $mmax = $m if $gmax->{hist}[$m] > $gmax->{hist}[$mmax];
}

say "#$gmax->{id} * $mmax = ", $gmax->{id} * $mmax;

for my $g ( values %gs ) {
    for my $m ( 0 .. 59 ) {
        if ( $g->{hist}[$m] > $gmax->{hist}[$mmax] ) {
            $mmax = $m;
            $gmax = $g;
        }
    }
}
say "#$gmax->{id} * $mmax = ", $gmax->{id} * $mmax;
