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

my @pats = ([]);

my $sol_a = 0;
my $sol_b = 0;

while ( <> )
{
    chomp;
    if ( /^$/ )
    {
        push @pats, [];
    }
    else
    {
        push $pats[ -1 ]->@*, $_;
    }
}

for my $p ( @pats )
{
    my @transposed = transpose( @$p );

    $sol_a += 100 * find_row_reflection( 0, @$p );
    $sol_a += find_row_reflection( 0, @transposed );

    $sol_b += 100 * find_row_reflection( 1, @$p );
    $sol_b += find_row_reflection( 1, @transposed );
}

say $sol_a;
say $sol_b;

sub transpose( @p )
{
    my @res;
    for my $x ( 0 .. ( length $p[ 0 ] ) - 1 )
    {
        $res[ $x ] = join '', map { substr $p[ $_ ], $x, 1 } 0 .. $#p;
    }
    return @res;
}

sub find_row_reflection( $tolerance, @p )
{
    REFLECTION:
    for my $l ( 1 .. $#p ) # 1 = between 0th and 1st row
    {
        my $smudges = 0;
        my ( $above, $below ) = ( $l - 1, $l );
        while ( $above >= 0 && $below < @p )
        {
            $smudges += hamming( $p[ $above ], $p[ $below ] );
            next REFLECTION if $smudges > $tolerance;
            -- $above;
            ++ $below
        }
        return $l if $smudges == $tolerance;
    }
}

sub hamming( $s1, $s2 )
{
    my $d = 0;
    for my $x ( 0 .. ( length $s1 ) - 1 )
    {
        ++ $d unless ( substr $s1, $x, 1 ) eq ( substr $s2, $x, 1 );
    }
    return $d;
}
