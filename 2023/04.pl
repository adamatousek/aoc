#!/usr/bin/env perl

use v5.36;
use strict;
use warnings;
use utf8;

no warnings qw/experimental::builtin/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product uniq/;

my $sol_a = 0;
my $sol_b = 0;

my @copies;

while ( <> )
{
    my ($id, $winning, $mine) = /(\d+):\s*((?:\d+\s*)*)\|\s*((?:\d+\s*)*)/;
    my $copies = 1 + ( $copies[ $id ] // 0 );

    my $re = join '|', split /\s+/, $winning;
    my $match_count = grep /^($re)$/, split /\s+/, $mine;

    my $value = $match_count ? 1 << ( $match_count - 1 ) : 0;
    $sol_a += $value;

    $copies[ $_ + $id ] += $copies for 1 .. $match_count;
    $sol_b += $copies;
}

say $sol_a;
say $sol_b;
