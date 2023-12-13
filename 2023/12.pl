#!/usr/bin/env perl

use v5.36;
use strict;
use warnings;
use utf8;

use experimental qw/builtin for_list/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product/;

my $sol_a = 0;
my $sol_b = 0;

while ( <> )
{
    my @wells = /[.?#]/g;
    my @runs = /\d+/g;
    $sol_a += solve( \@wells, \@runs );

    my @quintupled = ( @wells, '?' ) x 5;
    pop @quintupled;
    $sol_b += solve( \@quintupled, [ (@runs) x 5 ] );
}

say $sol_a;
say $sol_b;


sub solve( $wells, $runs )
{
    my @t;
    my $last_broken = -1;
    for my ( $i, $w ) ( indexed @$wells )
    {
        $last_broken = $i if $w eq '#';
    }

    for my $runs_left ( 1 .. @$runs )
    {
        my @runs = $runs->@[ -$runs_left .. -1 ];
        my $min_len = $#runs + sum @runs;
        for my $leftmost ( reverse 0 .. $#$wells )
        {
            my $when_not_used = $t[ $runs_left ][ $leftmost + 1 ] // 0;
            my $when_used = $runs_left > 1
                       ? ( $t[ $runs_left - 1 ][ $leftmost + $runs[ 0 ] + 1 ] // 0 )
                       : $leftmost + $runs[ 0 ] + 1 > $last_broken;

            my $must_use = $wells->[ $leftmost ] eq '#';
            my $cannot_use = ( @$wells - $leftmost < $min_len )
                || ( $leftmost > 0 && $wells->[ $leftmost - 1 ] eq '#' )
                || ( grep /\./, $wells->@[ $leftmost .. $leftmost + $runs[ 0 ] - 1 ] )
                || ( ( $wells->[ $leftmost + $runs[ 0 ] ] // '' ) eq '#' );

            my $v = \$t[ $runs_left ][ $leftmost ];

            $$v = 0;
            next if $must_use && $cannot_use;
            $$v += $when_not_used if !$must_use;
            $$v += $when_used if !$cannot_use;
        }
    }

    return sum $t[ -1 ][ 0 ];
}
