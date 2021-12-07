#!/usr/bin/perl

use strict;
use warnings;

use List::Util qw/min max/;

my $line = <>;
chomp $line;
my @nums = split /,/, $line;

my $size = 1 + max @nums;
my @costs_a = (0) x $size;
my @costs_b = (0) x $size;

my %crabs;
$crabs{$_}++ for @nums;

while ( my ($crab_pos, $crab_count) = each %crabs ) {
    for my $pos ( 0 .. $size ) {
        my $d = abs( $crab_pos - $pos );
        $costs_a[$pos] += $crab_count * $d;
        $costs_b[$pos] += $crab_count * ($d * ($d + 1) / 2);
    }
}

my $part_a = min @costs_a;
my $part_b = min @costs_b;
print "$part_a\n$part_b\n";
