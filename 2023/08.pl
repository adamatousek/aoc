#!/usr/bin/env perl

use v5.36;
use strict;
use warnings;
use utf8;

use experimental qw/builtin for_list/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product/;
use Math::Utils qw/gcd lcm/;

my @steps =<>=~ /[RL]/g;
my %net;

while ( <> )
{
    my @ns = /(\w{3})/g;
    next unless @ns == 3;

    $net{ shift @ns } = \@ns;
}

say solve( 'AAA', 'ZZZ' );
say solve( 'A$', 'Z$' );

# assumes that from each final node, no other final node is reacheble (with the
# directions given), only itself in a loop.
sub solve( $start, $end )
{
    my @ns;

    for my $n ( grep /$start/, keys %net )
    {
        my $nsteps = 0;
        while ( $n !~ /$end/ )
        {
            my $s = $steps[ $nsteps % @steps ];
            $n = $net{ $n }[ $s eq 'R' ];
            ++$nsteps;
        }
        push @ns, $nsteps;
    }

    # wtf
    return lcm @ns;
}
