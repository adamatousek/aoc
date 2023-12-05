#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my %ms;
while ( <> ) {
    /^(\w+):/ or next;
    my $m = { id => $1 };
    $ms{$1} = $m;
    $m->{val} = 0+$1 if /(\d+)/;
    if ( /(\w+) ([-\+\*\/]) (\w+)/ ) {
        $m->{l} = $1;
        $m->{op} = $2;
        $m->{r} = $3;
    }
}


say solve( 'root' );

sub solve($id) {
    my $m = $ms{$id};
    return $m->{val} if defined $m->{val};
    my @sub = map solve($_), $m->@{'l','r'};
    my $v = $m->{op} eq '+' ? $sub[0] + $sub[1]
          : $m->{op} eq '-' ? $sub[0] - $sub[1]
          : $m->{op} eq '*' ? $sub[0] * $sub[1]
          : $m->{op} eq '/' ? $sub[0] / $sub[1]
          : die 'unknown op';
    $m->{val} = $v;
    return $v;
}
