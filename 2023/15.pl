#!/usr/bin/env perl

use v5.36;

use experimental qw/builtin for_list/;
use builtin qw/indexed/;

use List::Util qw/sum/;


chomp( my $in = <> );
my @steps = split ',', $in;

say sum map hash( $_ ), @steps;

my @boxes;
foreach ( @steps )
{
    my ( $label, $lens ) = /([a-z]+)[=-](\d)?/;
    my $h = hash( $label );
    my $ix = find( $label, $boxes[ $h ] //= [] );
    splice $boxes[ $h ]->@*, $ix, 1, $lens ? [ $label, $lens ] : ();
}

my $sol_b = 0;
for my ( $bi, $b ) ( indexed @boxes )
{
    for my ( $slot, $lens ) ( indexed @$b )
    {
        $sol_b += ( 1 + $bi ) * ( 1 + $slot ) * $lens->[1];
    }
}
say $sol_b;

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sub hash( $s )
{
    my $h = 0;
    $h = 17 * ( $h + ord ) % 256 for $s =~ /./g;
    return $h;
}

sub find( $v, $vs )
{
    my $ix = 0;
    ++ $ix while $ix < @$vs && $vs->[ $ix ][ 0 ] ne $v;
    return $ix;
}
