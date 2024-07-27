#!/usr/bin/env perl

use v5.36;

my ($x, $y) = (0, 0);

my $state_a = { x => 0, y => 0, area => 1 };
my $state_b = { x => 0, y => 0, area => 1 };

while ( <> )
{
    my ( $dir, $len, $hlen, $hdir ) = /(.) (\d+) \(#(.{5})(.)\)/;
    shoelace( $state_a, $dir, $len );

    $hdir =~ y/0123/RDLU/;
    shoelace( $state_b, $hdir, hex $hlen );
}

say $state_a->{area};
say $state_b->{area};

sub shoelace( $st, $d, $l )
{
    if ( $d =~ /[LU]/ ) { $st->{area} += $l }

    if ( $d eq 'U' ) { $st->{y} += $l; return }
    if ( $d eq 'D' ) { $st->{y} -= $l; return }

    $l = -$l if $d eq 'L';
    $st->{area} += $l * $st->{y};
    $st->{x} += $l;
}
