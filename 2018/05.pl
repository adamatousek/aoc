#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;
use List::Util qw/min/;

my $s = <>;
chomp $s;
my @s = reduce( split //, $s );

my $best = scalar @s;
say $best;

for my $bad ( 'a' .. 'z' ) {
    my @s2 = reduce( grep lc $_ ne $bad, @s );
    $best = min( $best, scalar @s2 );
}
say $best;

sub reduce {
    my @s;
    for ( @_ ) {
        push @s, $_;
        splice @s, -2 if @s > 1 and lc $s[-1] eq lc $s[-2] and $s[-1] ne $s[-2];
    }
    return @s;
}
