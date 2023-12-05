#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use List::Util qw/max/;

my %map = qw/X A Y B Z C/;
my %result = qw/AA 3 BB 3 CC 3 BA 6 CB 6 AC 6/;
my %map2 = qw/XA C XC B XB A YA A YB B YC C ZA B ZB C ZC A/;

my ($total1, $total2);
while ( <> ) {
    my ($them, $us0) = /^(.) (.)$/ or next;
    my $us = $map{$us0};
    my $us2 = $map2{"$us0$them"};
    $total1 += score($us) + ( $result{"$us$them"} // 0);
    $total2 += score($us2) + ( $result{"$us2$them"} // 0);
}

say $total1;
say $total2;

sub score($x) { 1 + ord($x) - ord('A') }
