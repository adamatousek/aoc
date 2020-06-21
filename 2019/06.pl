#!/usr/bin/perl
use v5.16;
use strict;
use List::Util qw[min];
use warnings;
no warnings 'experimental';
no warnings 'recursion';

our %tree = ();

while ( <> =~ /(...)\)(...)/ ) {
    if ( not exists( $tree{$1} ) ) {
        $tree{$1} = { out => [], p => $1, hops => 0 };
    }
    if ( not exists( $tree{$2} ) ) {
        $tree{$2} = { out => [], p => $1, hops => 0 };
    }
    push (@{$tree{$1}{out}}, $2);
    $tree{$2}{p} = $1;
}

our $count;
count_orbits( "COM", 0 );
say "Count: ", $count;

our %vis = ();
our $hops;
count_hops( "YOU", "SAN", 0 );
say "Hops: ", $hops - 2;

sub count_orbits {
    my ($v, $p) = @_;
    $count += $p;
    foreach my $s ( @{$tree{$v}{out}} ) {
        count_orbits( $s, $p + 1 );
    }
}

sub count_hops {
    my ($from, $to, $acc) = @_;
    return if exists( $vis{$from} ) && $vis{$from} == 1;
    $vis{$from} = 1;
    return $hops = $acc if $from eq $to;
    my $v = $tree{$from};
    count_hops($_, $to, $acc + 1) for $v->{p}, @{$v->{out}};
}
