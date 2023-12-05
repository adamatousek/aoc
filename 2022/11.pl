#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/product/;

my @in = <>;

for my $part_b ( 0, 1 ) {
    my @m = parse( @in );
    my $mod = product map { $_->{test} } @m;

    for my $round ( 1 .. ( $part_b ? 10_000 : 20 ) ) {
        for my $m ( @m ) {
            for my $i ( $m->{items}->@* ) {

                $i *= $i if $m->{sq};
                $i *= $m->{coef};
                $i += $m->{add};
                $i = int( $i / 3 ) unless $part_b;
                $i %= $mod if $part_b;
                $m->{count}++;

                my $test_result = $i % $m->{test} == 0 ? 'true' : 'false';
                my $dst = $m->{$test_result};
                push $m[$dst]{items}->@*, $i;
            }
            $m->{items} = [];
        }
    }

    @m = sort { $b <=> $a } map { $_->{count} } @m;
    say product( @m[0,1] );
}


sub parse {
    my $m;
    my @m;
    for ( @_ ) {
        if ( /^Monkey/ ) {
            $m = { id => scalar @m };
            push @m, $m;
        } elsif ( /Starting items/ ) {
            $m->{items} = [ /\d+/g ];
        } elsif ( /Operation:/ ) {
            $m->{sq}   = /old \* old/   ?  1 : 0;
            $m->{coef} = /old \* (\d+)/ ? $1 : 1;
            $m->{add}  = /old \+ (\d+)/ ? $1 : 0;
        } elsif ( /Test: divisible by (\d+)/ ) {
            $m->{test} = $1;
        } elsif ( /If (true|false): throw to monkey (\d+)/ ) {
            $m->{$1} = $2;
        } else {
            die "Wtf is $_" unless /^$/;
        }
    }
    return @m;
}
