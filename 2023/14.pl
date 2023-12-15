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

my @cols; # south toward higher indices

while ( <> )
{
    chomp;
    while ( /./g )
    {
        push $cols[ pos() - 1 ]->@*, $&;
    }
}

my $n_rounds = 4 * 1_000_000_000;
for ( my $round = 1; $round <= $n_rounds; ++$round )
{
    my $seen = tilt_north( $round );
    say north_load() if $round == 1;
    rotate();

    if ( $seen )
    {
        my $loop_len = $round - $seen;
        $round = $n_rounds - ( $n_rounds - $seen ) % $loop_len;
    }
}

say north_load();

##############################################################################

sub tilt_north( $round )
{
    state %seen;

    my $state;

    for my $c ( @cols )
    {
        my $key = join '', @$c;
        $state .= $key;

        my $dst = 0;
        my $src = 0;

        while ( $src < @$c )
        {
            if ( $dst >= $src )
            {
                $src = $dst + 1;
            }
            elsif ( $c->[ $dst ] ne '.' )
            {
                ++ $dst;
            }
            elsif ( $c->[ $src ] eq '#' )
            {
                $dst = $src;
            }
            elsif ( $c->[ $src ] eq 'O' )
            {
                $c->[ $dst ] = 'O';
                $c->[ $src ] = '.';
            }
            else
            {
                ++ $src;
            }
        }
    }

    my $seen_ref = \$seen{ $state };
    my $seen_at = $$seen_ref // 0;
    $$seen_ref = $round;
    return $seen_at;
}

sub north_load()
{
    my $sol = 0;
    for my $c ( @cols )
    {
        for ( 0 .. $c->$#* )
        {
            $sol += $c->@* - $_ if $c->[ $_ ] eq 'O'
        }
    }
    return $sol;
}

sub rotate()
{
    my @new;
    my $w = $#cols;
    my $h = $cols[ 0 ]->$#*;

    for my $x ( 0 .. $w )
    {
        for my $y ( 0.. $h )
        {
            $new[ $h - $y ][ $x ] = $cols[ $x ][ $y ];
        }
    }

    @cols = @new;
}
