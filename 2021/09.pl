#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use List::Util qw/product/;

my @map = ([]);

while (<>) {
    chomp;
    push @map, [undef, map +{ h => $_ }, /(\d)/g];
}

my $width = $map[1]->$#*;
my $basin_id = 1;
my %basin_sizes;
my $sum_a = 0;

for my $y (1 .. $#map) {
    for my $x (1 .. $width) {
        my $p = $map[$y][$x];
        my $h = $p->{h};
        my $lower = 1;
        for my $n ( $map[$y][$x-1], $map[$y][$x+1],
                    $map[$y-1][$x], $map[$y+1][$x] ) {
            next unless defined $n->{h};
            $lower &&= $h < $n->{h};
        }
        if ( $lower ) {
            $sum_a += 1 + $h;
            $basin_sizes{$basin_id} = 1;
            $p->{basin} = $basin_id++;
            flood_out( $y, $x );
        }
    }
}

my @sizes = sort {$b <=> $a} values %basin_sizes;
my $prod_b = product @sizes[0 .. 2];

print "$sum_a\n$prod_b\n";

sub flood_out {
    my ($y, $x) = @_;
    my $p = $map[$y][$x];
    $p->{active} = 1;
    for my $ds ( [0, 1], [0, -1], [1, 0], [-1, 0] ) {
        my ($ny, $nx) = ( $y + $ds->[0], $x + $ds->[1] );
        my $n = $map[$ny][$nx];
        next unless defined $n->{h};
        next if $n->{h} == 9;
        next if $n->{active};
        next if $n->{basin};
        next unless $n->{h} > $p->{h};
        flow_down( $ny, $nx ) and flood_out( $ny, $nx );
    }
    $p->{active} = 0;
}

sub flow_down {
    my ($y, $x) = @_;
    my $p = $map[$y][$x];
    my @candidates;

    for my $ds ( [0, 1], [0, -1], [1, 0], [-1, 0] ) {
        my ($ny, $nx) = ( $y + $ds->[0], $x + $ds->[1] );
        my $n = $map[$ny][$nx];
        next unless defined $n->{h};
        next unless $n->{h} < $p->{h};
        push @candidates, $n;
    }

    return 0 if @candidates == 0; # shouldn't happen
    my %candidate_basins;
    $candidate_basins{$_->{basin} // 0} = 1 for @candidates;
    if ( keys %candidate_basins == 1 && ! $candidate_basins{0} ) {
        $p->{basin} = $candidates[0]->{basin};
        $basin_sizes{$p->{basin}}++;
        return 1;
    }
    return 0;
}
