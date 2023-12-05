#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my @dirs = ( [1,0,0], [0,1,0], [0,0,1] );
push @dirs, map [ map -$_, @$_ ], @dirs;

my @in = map { chomp; [ map $_ + 1, split ',' ] } <>;

my @space;
my @max_size;
my $res_a = 0;
for my $c ( @in ) {
    $res_a += 6;
    $space[$c->[0]][$c->[1]][$c->[2]] = 1;
    for my $d ( @dirs ) {
        my $c2 = vecadd( $c, $d );
        $res_a -= 2
            if $space[$c2->[0]][$c2->[1]][$c2->[2]];
        $max_size[$_] = max 1 + $c->[$_], $max_size[$_] // 0
            for 0 .. 2;
    }
}
say $res_a;

my $res_b = 0;
my @q = ([0, 0, 0]);
$space[0][0][0] = 2;
while ( @q ) {
    my @todo;
    for my $c ( @q ) {
        my ($x, $y, $z) = @$c;
        NEIGH:
        for my $d ( @dirs ) {
            my $c2 = vecadd( $c, $d );
            for ( 0 .. 2 ) {
                next NEIGH unless 0 <= $c2->[$_] <= $max_size[$_];
            }

            my $n = \$space[$c2->[0]][$c2->[1]][$c2->[2]];
            $res_b++ if $$n && $$n == 1;
            next if $$n;
            $$n = 2;
            push @todo, $c2;
        }
    }
    @q = @todo;
}

say $res_b;

sub vecadd($a, $b) { [map $a->[$_] + $b->[$_], 0 .. $a->$#*] }
