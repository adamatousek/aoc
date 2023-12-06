#!/usr/bin/env perl

use v5.36;
use utf8;

use experimental qw/builtin for_list/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product zip unpairs/;
use Math::Utils qw/ceil floor/;

my @times =<>=~ /\d+/g;
my @dists =<>=~ /\d+/g;

my $sol_a = 1;
for my ($t, $d) ( unpairs zip \@times, \@dists )
{
    $sol_a *= count_wins( $t, $d );
}
say $sol_a;

$" = '';
say count_wins( "@times", "@dists" );


sub count_wins( $t, $d )
{
    # distance between roots of x * (t - x) - d = 0
    my $D = ($t * $t) - 4 * $d;
    my $x1 = ( - $t + sqrt( $D ) ) / -2;
    my $x2 = ( - $t - sqrt( $D ) ) / -2;
    # ew.
    $x1 = ($x1 == int( $x1 ) ) ? $x1 + 1 : ceil( $x1 ); # first winning x
    $x2 = ($x2 == int( $x2 ) ) ? $x2 - 1 : floor( $x2 ); # last winning x
    return 1 + $x2 - $x1;
}
