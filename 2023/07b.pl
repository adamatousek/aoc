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

my %cards = reverse indexed split //, 'J23456789TQKA';

print Dumper \%cards;

chomp( my @hands = <> );


my @by_strength = sort { hand_type( $a ) <=> hand_type( $b )
                                          ||
                      hand_strxfrm( $a ) cmp hand_strxfrm( $b ) } @hands;
my $sol_b = 0;

foreach my ($rank, $bid) ( indexed @by_strength ) {
    $bid =~ s/\w{5} //;
    $sol_b += ( $rank + 1 ) * $bid;
}

say $sol_b;


sub hand_strxfrm( $h )
{
    $h =~ s/ .*//;
    return join '', map chr( ord( 'a' ) + $cards{ $_ } ), split //, $h;
}

sub hand_type( $h )
{
    $h =~ s/ .*//;
    my $js = $h =~ s/J//g;

    my %counts;
    $counts{$_}++ for split //, $h;
    my @c = sort { $b <=> $a } values %counts;
    push @c, 0, 0;

    return 6 if $c[ 0 ] + $js == 5;
    return 5 if $c[ 0 ] + $js == 4;
    return 4 if $c[ 0 ] + $js == 3 && $c[ 1 ] == 2;
    return 3 if $c[ 0 ] + $js == 3;
    return 2 if $c[ 0 ] + $js == 2 && $c[ 1 ] == 2;
    return 1 if $c[ 0 ] + $js == 2;
    return 0;
}
