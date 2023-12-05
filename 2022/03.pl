#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use List::Util qw/max/;

my $res_a;
my $res_b;

my %seen_group;

while ( <> ) {
    chomp;
    my @fst = split //;


    my %seen_b;
    $seen_b{$_}++ for @fst;
    $seen_group{$_}++ for keys %seen_b;
    if ( $. % 3 == 0 ) {
        $res_b += score( { reverse %seen_group }->{3} );
        %seen_group = ();
    }

    my @snd = splice @fst, scalar @fst / 2;
    my %seen;
    $seen{$_}++ for @fst;
    for ( @snd ) {
        if ( $seen{$_} ) {
            $res_a += score( $_ );
            last;
        }
    }
}

say $res_a;
say $res_b;

sub score {
    for ( shift ) {
        return  1 + ord() - ord('a') if /[a-z]/;
        return 27 + ord() - ord('A') if /[A-Z]/;
    }
    die;
}
