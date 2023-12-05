#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/none all/;

$/ = '';
my $res_a;
my @packets;
while ( <> ) {
    my @p = map parse( $_ ), split /\n/;
    $res_a += $. if compare( @p ) <= 0;
    push @packets, @p;
}

push @packets, [[2]], [[6]];
@packets = sort { compare( $a, $b ) } @packets;

my $res_b = 1;
while ( my ($i, $p) = each @packets ) {
    $res_b *= $i + 1 if @$p == 1
                     && ref $p->[0] eq 'ARRAY'
                     && $p->[0]->@* == 1
                     && ( $p->[0][0] == 2 || $p->[0][0] == 6 )
}

say $res_a;
say $res_b;

sub parse($s) {
    my @stack;
    while ( $s =~ /(\d+|\[|\])/g ) {
        if ( $1 eq '[' ) {
            push @stack, [];
        }
        elsif ( $1 eq ']' ) {
            my $x = pop @stack;
            if ( @stack ) {
                push $stack[-1]->@*, $x;
            }
            else {
                return $x;
            };
        }
        else {
            push $stack[-1]->@*, $1;
        }
    }
}

sub compare($a, $b) {
    my @is_list = map { ref eq 'ARRAY' } $a, $b;
    return $a <=> $b if none {$_} @is_list;
    $a = [$a] unless $is_list[0];
    $b = [$b] unless $is_list[1];
    my $i = 0;
    while ( $i < @$a && $i < @$b ) {
        my $c = compare( $a->[$i], $b->[$i] );
        return $c if $c;
        ++$i;
    }
    return @$a <=> @$b;
}

