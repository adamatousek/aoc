#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

my $res_a;
my $res_b;
while ( <> ) {
    chomp;
    my @n = split /[-,]/;
    @n = @n[2,3,0,1] if $n[0] > $n[2] || $n[0] == $n[2] && $n[1] < $n[3];
    $res_a++ if $n[2] <= $n[3] <= $n[1];
    $res_b++ if $n[2] <= $n[1];
}
say $res_a;
say $res_b;
