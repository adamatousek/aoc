#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my @in = <>;

my @m = parse( @in );
print Dumper \@m;

for my $round ( 1 .. 10_000 ) {
    for my $m ( @m ) {
        for my $i ( $m->{items}->@* ) {
            $i *= $i if $m->{sq};
            $i *= $m->{coef};
            $i += $m->{add};
            #$i = int( $i / 3 );
            $m->{count}++;

            my $test_result = $i % $m->{test} == 0 ? 'true' : 'false';
            my $dst = $m->{$test_result};
            push $m[$dst]{items}->@*, $i;
        }
        $m->{items} = [];
    }
}

my ($res_a1, $res_a2) = sort { $b <=> $a } map { $_->{count} } @m;
say $res_a1 * $res_a2;


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
