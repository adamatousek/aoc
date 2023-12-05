#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;
use List::Util qw/min max/;

chomp(my $in = <>);
