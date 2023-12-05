#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;

my @in = <>;
my $freq = 0;
my %seen = ( 0 => 1 );
my $seen;
my $sol_a;
while (!defined $seen) {
    foreach ( @in ) {
        $freq += $_;
        $seen = $freq if $seen{$freq}++ && !defined $seen;
    }
    $sol_a = $freq unless defined $sol_a;
}
say $sol_a;
say $seen;
