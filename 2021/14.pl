#!/usr/bin/perl

use warnings;
use strict;
use List::Util qw/min max/;

my $l = <>;
my @tmpl = $l =~ /\w/g;
<>;
my %rules;
while ( <> ) {
    /(\w\w) -> (\w)/ or die;
    $rules{$1} = $2;
}

my %memo;

for my $depth ( 10, 40 ) {
    my %freq;

    my $x = $tmpl[0];
    $freq{$x}++;
    foreach ( @tmpl[1..$#tmpl] ) {
        $freq{$_}++;
        add_to_hash( \%freq, go_pair( $depth, $x, $_, \%freq ) );
        $x = $_;
    }

    my $answer = max ( values %freq ) - min ( values %freq );
    print "depth $depth: $answer\n";
}

sub go_pair {
    my ($d, $x, $y, $freq) = @_;
    return if $d == 0;
    my $key = "$d,$x$y";
    return $memo{$key} if ( $memo{$key} );

    my $mid = $rules{"$x$y"};
    my $freqs = {};
    return unless $mid;
    $freqs->{$mid}++;
    add_to_hash( $freqs, go_pair( $d - 1, $x, $mid, $freq ) );
    add_to_hash( $freqs, go_pair( $d - 1, $mid, $y, $freq ) );
    $memo{$key} = $freqs;
    return $freqs;
}

sub add_to_hash {
    my ($a, $b) = @_;
    while ( my ($k, $v) = each %$b ) {
        $a->{$k} += $v;
    }
}
