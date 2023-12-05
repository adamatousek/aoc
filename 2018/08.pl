#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;
use List::Util qw/sum/;

my @in = <> =~ /\d+/g;

my $ix = 0;
my $sum;

my $root_val = go();
say $sum;
say $root_val;

sub go {
    my ($children, $metas) = @in[$ix, $ix + 1];
    $ix += 2;
    my @subvals = map go(), 1 .. $children;
    my @metadata = @in[$ix .. $ix + $metas - 1];
    my $metasum = sum @metadata;
    $sum += $metasum;
    $ix += $metas;
    return $children ? sum @subvals[ map $_ - 1, grep { 1 <= $_ <= @subvals } @metadata ] : $metasum;
}
