#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use List::Util qw/max sum/;

$/ = '';
my @xs = sort { $b <=> $a } map { sum split } <>;
say $xs[0];
say sum @xs[0..2];
