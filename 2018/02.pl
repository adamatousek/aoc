#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;

my ($c2, $c3, $common);
my %match;
while ( <> ) {
    chomp;
    my %f;
    $f{$_}++ for split //;
    %f = reverse %f;
    $c2++ if $f{2};
    $c3++ if $f{3};

    for my $i (1 .. length) {
        my $s = $_;
        substr $s, $i - 1, 1, '_';
        $common = $s =~ s/_//r if $match{$s}++;
    }
}

say $c2 * $c3;
say $common;
