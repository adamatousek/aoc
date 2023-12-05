#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my $zero;
my @in = map { chomp; { n => $_} } <>;
for my $i ( 0 .. $#in ) {
    my $n1 = $in[$i-1];
    my $n2 = $in[$i];
    $zero = $n2 if $n2->{n} == 0;
    $n1->{next} = $n2;
    $n2->{prev} = $n1;
}

my $len = @in;
for my $n ( @in ) {
    my $d = $n->{n} % ( $len - 1 );
    $d += $len if $d < 0;
    next if $d == 0;

    my $prev = $n->{prev};
    my $next = $n->{next};
    $prev->{next} = $next;
    $next->{prev} = $prev;

    while ( $d --> 0 ) {
        $prev = $next;
        $next = $next->{next};
    }

    $prev->{next} = $n;
    $next->{prev} = $n;
    $n->{prev} = $prev;
    $n->{next} = $next;
}

my $res_a;
for ( 1000, 2000, 3000 ) {
    my $off = $_ % $len;
    my $c = $zero;
    $c = $c->{next} while $off --> 0;
    say "*** $c->{n}";
    $res_a += $c->{n};
}
say $res_a;
