#!/usr/bin/env perl

use v5.36;
use strict;
use warnings;
use utf8;

no warnings qw/experimental::builtin/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product uniqint pairs/;

my @seeds =<>=~ /\d+/g;
my @maps;

while ( <> )
{
    next unless /./;
    if ( /map/ )
    {
        push @maps, [];
    }
    else
    {
        push @{$maps[-1]}, [ /\d+/g ];
    }
}

my $sol_a = 'inf';
my $sol_a2 = 'inf';
my $sol_b = 'inf';

$sol_a = min remap( $_, 0 ), $sol_a for @seeds;
$sol_a2 = min remapped_min( $_, $_ + 1, 0 ), $sol_a2 for @seeds;
$sol_b = min remapped_min( $_->[0], $_->[0] + $_->[1], 0 ), $sol_b for pairs @seeds;

say $sol_a;
say $sol_a2, " (check)";
say $sol_b;

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sub remapped_min( $begin, $end, $level )
{
    return $begin unless $level < @maps;
    my @starts = sort { $a <=> $b } uniqint( $begin, $end,
        grep $begin < $_ < $end, map { $_->[1], $_->[1] + $_->[2] } @{ $maps[ $level ] }
    );

    my @subs;

    SUBINTERVAL:
    for ( 0 .. $#starts - 1 )
    {
        my $sub_begin = $starts[ $_ ];
        my $sub_end = $starts[ $_ + 1 ];

        my $shift = 0;
        for my $m ( @{ $maps[ $level ] } )
        {
            my ($dst, $src, $len ) = @$m;
            next unless $src <= $sub_begin < $src + $len;
            die unless $src < $sub_end <= $src + $len;
            $shift = $dst - $src;
            last;
        }

        push @subs, remapped_min( $sub_begin + $shift, $sub_end + $shift, $level + 1 );
    }

    return min @subs;
}

sub remap( $v, $level )
{
    return $v unless $level < @maps;
    for my $m ( @{ $maps[ $level ] } )
    {
        my ($dst, $src, $len ) = @$m;
        return remap( $dst + ( $v - $src ), $level + 1 )
            if $src <= $v < $src + $len;
    }
    return remap( $v, $level + 1 );
}
