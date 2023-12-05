#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;
use List::Util qw/max product/;

my %cap = qw/red 12 green 13 blue 14/;
my $sol_a = 0;
my $sol_b = 0;

LINE:
while ( <> )
{
    my ($id) = /Game (\d+)/;
    my %maxes;
    my $possible = 1;
    for my $r ( split /;/ )
    {
        my %bag;
        for my $c ( qw/red green blue/ )
        {
            $r =~ s|(\d+) $c|$bag{$c} += $1; $maxes{$c} = max $1, $maxes{$c} // 0|ge;
            $possible = 0 if ( $bag{$c} // 0 ) > $cap{$c};
        }
    }
    $sol_a += $id if $possible;
    $sol_b += product values %maxes;
}

say $sol_a;
say $sol_b;
