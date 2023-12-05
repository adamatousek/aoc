#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my $res = 0;
while ( <> ) {
    chomp;
    my $num = parse( $_ );
    say "** $_  =>  $num";
    $res += $num;
}
say "** $res";
say encode($res);

sub parse($s) {
    my $n = 0;
    for my $d ( $s =~ /./g ) {
        $n *= 5;
        if ( $d eq '-' ) {
            $n -= 1;
        }
        elsif ( $d eq '=' ) {
            $n -= 2;
        }
        else {
            $n += $d;
        }
    }
    return $n;
}

sub encode($n) {
    my $s = '';
    while ( $n ) {
        my $d = $n % 5;
        $d -= 5 if $d > 2;
        $n -= $d;
        $n = int( $n / 5 );
        $s = $d  . $s if $d >= 0;
        $s = '-' . $s if $d == -1;
        $s = '=' . $s if $d == -2;
    }
    return $s;
}
