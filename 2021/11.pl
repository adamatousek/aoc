#!/usr/bin/perl

use warnings;
use strict;

my @octopi;
my $flashes = 0;
my $steps = 0;

for ( 1..10 ) {
    my $l = <>;
    chomp $l;
    push @octopi, $l =~ /\d/g;
}

while ( 1 ) {
    my $f = step( \@octopi );
    $flashes += $f;
    ++$steps;
    print "$flashes\n" if $steps == 100;
    if ( $f == 100 ) {
        print "$steps\n";
        last;
    }
}

sub neighs {
    my $i = shift;
    my @ns = ( -10, 10 );
    push @ns, ( -11, -1, 9 ) unless $i % 10 == 0;
    push @ns, (  -9, 11, 1 ) unless $i % 10 == 9;
    return grep { $_ >=0 && $_ < 100 } map { $_ + $i } @ns;
}

sub step {
    my $state = shift;
    my $f = 0;
    foreach ( @$state ) {
        $_ += 1;
    }
    for my $i ( 0 .. $#$state ) {
        $f += step_one( $state, $i );
    }
    return $f;
}

sub step_one {
    my ( $state, $i ) = @_;
    return 0 if $state->[$i] <= 9;
    $state->[$i] = 0;
    my $f = 1;
    for my $n ( neighs( $i ) ) {
        $state->[$n]++ if $state->[$n];
        $f += step_one( $state, $n );
    }
    return $f;
}

sub print_octopi {
    for ( 0.. 9 ) {
        print @_[10 * $_ .. 10 * $_ + 9], "\n";
    }
    print "\n";
}
