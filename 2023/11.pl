#!/usr/bin/env perl

use v5.36;
use strict;
use warnings;
use utf8;

my @gs;
my @row_used;
my @col_used;

while ( <> )
{
    while ( /#/g )
    {
        push @gs, [ pos, $. ];
        $col_used[ pos ] = 1;
        $row_used[ $. ] = 1;
    }
}

for my $expansion ( 2, 1_000_000 )
{
    my @row_map = remap( $expansion, @row_used );
    my @col_map = remap( $expansion, @col_used );

    my $sol = 0;
    for my $i ( 0 .. $#gs )
    {
        my ($x1, $y1) = $gs[$i]->@*;
        for my $j ( $i + 1 .. $#gs )
        {
            my ($x2, $y2) = $gs[$j]->@*;
            $sol += abs( $col_map[$x1] - $col_map[$x2] )
                  + abs( $row_map[$y1] - $row_map[$y2] );
        }
    }

    say $sol;
}


sub remap( $expansion, @used )
{
    my @map;
    my $dst = 1;
    for my $src ( 0 .. $#used )
    {
        $map[ $src ] = $dst;
        $dst += $used[ $src ] ? 1 : $expansion;
    }
    return @map;
}
