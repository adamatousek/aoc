#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my @in = map { chomp; { n => $_ } } <>;

say solve( \@in, 1, 1 );
say solve( \@in, 811589153, 10 );
exit;

sub solve($in, $factor, $repeats) {
    my @in = @$in;
    my $zero;

    # Build linked list
    for my $i ( 0 .. $#in ) {
        my $n1 = $in[$i-1];
        my $n2 = $in[$i];
        $zero = $n2 if $n2->{n} == 0;
        $n1->{next} = $n2;
        $n2->{prev} = $n1;
    }

    for ( 1 .. $repeats ) {
        for my $n ( @in ) {
            my $d = $factor * $n->{n} % ( @in - 1 );
            next if $d == 0;
            $d -= @in - 1 if $d > @in / 2;

            my $prev = $n->{prev};
            my $next = $n->{next};
            $prev->{next} = $next;
            $next->{prev} = $prev;

            while ( $d > 0 ) {
                $prev = $next;
                $next = $next->{next};
                $d--;
            }
            while ( $d < 0 ) {
                $next = $prev;
                $prev = $prev->{prev};
                $d++
            }

            $prev->{next} = $n;
            $next->{prev} = $n;
            $n->{prev} = $prev;
            $n->{next} = $next;
        }
    }

    my $res = 0;
    for ( 1000, 2000, 3000 ) {
        my $off = $_ % @in;
        my $c = $zero;
        $c = $c->{next} while $off --> 0;
        $res += $factor * $c->{n};
    }
    return $res;
}
