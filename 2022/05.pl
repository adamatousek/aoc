#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use Clone qw/clone/;

$/ = "";
my @rows = map {[/[[ ]([A-Z ])[] ] ?/g]} split "\n", <>;
pop @rows;

my @s_a;
for my $r ( reverse @rows ) {
    while ( my ($i, $crate) = each @$r ) {
        next unless $crate =~ /[A-Z]/;
        push $s_a[$i]->@*, $crate;
    }
}
my @s_b = @{ clone \@s_a };

$/ = "\n";
while ( <> ) {
    /move (\d+) from (\d+) to (\d+)/ or next;
    my ($n, $src, $dst) = ($1, $2 - 1, $3 - 1);

    my @crates = splice $s_b[$src]->@*, -$n;
    push $s_b[$dst]->@*, @crates;

    while ( $n --> 0 ) {
        my $crate = pop $s_a[$src]->@*;
        push $s_a[$dst]->@*, $crate;
    }
}

for ( \@s_a, \@s_b ) {
    print $_->[-1] for @$_;
    print "\n";
}
