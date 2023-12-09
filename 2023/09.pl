#!/usr/bin/env perl

use v5.36;
use strict;
use warnings;
use utf8;

my $sol_a = 0;
my $sol_b = 0;

while ( <> )
{
    my @ns = /-?\d+/g;
    $sol_a += extrapolate( @ns );
    $sol_b += extrapolate( reverse @ns );
}

say $sol_a;
say $sol_b;

sub extrapolate( @ns )
{
    return 0 unless grep $_, @ns;
    return $ns[-1] + extrapolate( map $ns[ $_ ] - $ns[ $_ - 1 ], 1 .. $#ns );
}
