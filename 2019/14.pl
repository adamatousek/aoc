#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
use integer;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ min max all ];
use Math::Utils qw[ gcd lcm ];

my %reactions;

while (<> =~ /^(.*) => (\d+) (.*)$/) {
    my %ings;
    my $res = $3;
    my $rec = $1;
    my $amount = $2;
    $rec =~ s/(\d+) ([A-Z]+)/$2, $1/g;
    %ings = split /, /, $rec;
    $reactions{$res} = {amount => $amount, ings => \%ings};
}

my %extra;

my $ore = calc_ore( 'FUEL', 1 );
say "Ore: ", $ore;


my $max_fuel = 3566500; # by a manual binary search, because it was faster
do {
    ++$max_fuel;
    %extra = ();
} while ( calc_ore( 'FUEL', $max_fuel ) < $max_ore );

say "Max fuel: ", $max_fuel - 1;

sub calc_ore {
    my ($targ, $amount) = @_;
    my $recipe = $reactions{$targ};

    return $amount if $targ eq 'ORE';

    if ( defined $extra{$targ} && $extra{$targ} > 0) {
        my $free = min ($amount, $extra{$targ} );
        $amount -= $free;
        $extra{$targ} -= $free;
        return 0 if $amount == 0;
    }

    my $coeff = ($amount + $recipe->{amount} - 1) / $recipe->{amount};
    my $remains = ($coeff * $recipe->{amount}) - $amount;
    my $cost = 0;
    foreach my $ing (keys(%{$recipe->{ings}})) {
        $cost += calc_ore( $ing, $coeff * $recipe->{ings}->{$ing} );
    }
    if ( $remains > 0 ) {
        $extra{$targ} = 0 if not defined $extra{$targ};
        $extra{$targ} += $remains;
    }
    return $cost;
}
