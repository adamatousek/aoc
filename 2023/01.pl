#!/usr/bin/env perl
use v5.36;
use strict;
use warnings;
use utf8;

no warnings qw/experimental::builtin/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;
use List::Util qw/max sum/;

my @digits = qw/zero one two three four five six seven eight nine/;
my %digits = reverse indexed( @digits );

my $re = qr/\d|@{[ join '|', @digits ]}/;

my $sum_a = 0;
my $sum_b = 0;

while ( <> ) {
    ($a) = /(\d)/;
    ($b) = /.*(\d)/;

    $sum_a += "$a$b";

    ($a) = /($re)/;
    ($b) = /.*($re)/;

    $a = $digits{$a} // $a;
    $b = $digits{$b} // $b;

    $sum_b += "$a$b";
}

say $sum_a;
say $sum_b;
