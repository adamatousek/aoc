#!/usr/bin/env perl

use v5.36;
use strict;
use warnings;
use utf8;

no warnings qw/experimental::builtin/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product/;

my $sol_a = 0;
my $sol_b = 0;

my %symbols;
my %numbers;

while ( <> )
{
    while ( /\d+/g )
    {
        my $n = { num => 0+$&, row => $., first_col => $-[0], last_col => $+[0] - 1 };
        $numbers{ "$. $_" } = $n for $-[0] .. $+[0] - 1;
    }
    while ( /[^\s\d.]/g )
    {
        my $s = { sym => $&, row => $., col => $-[0] };
        $symbols{ "$. $-[0]" } = $s;
    }
}

for my $s ( values %symbols )
{
    my $adjacent_count = 0;
    my $adjacent_product = 1;
    for my $coord ( around( $s->@{qw/row col/} ) )
    {
        my $n = $numbers{ $coord };
        next if !$n || $n->{seen}++;
        # there are no numbers with multiple symbols touching them
        $adjacent_count++;
        $sol_a += $n->{num};
        $adjacent_product *= $n->{num};
    }

    $sol_b += $adjacent_product
        if $adjacent_count == 2 and $s->{sym} eq '*';
}

say $sol_a;
say $sol_b;

sub around( $x, $y )
{
    my @res;
    for my $dx ( -1, 0, 1 )
    {
        for my $dy ( -1, 0, 1 )
        {
            next if $dx == $dy == 0;
            push @res, "@{[ $x + $dx, $y + $dy ]}";
        }
    }
    return @res;
}
