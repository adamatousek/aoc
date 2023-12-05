#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max min all/;

my @DIRS = ( [0, -1], [0, 1], [-1, 0], [1, 0] );

my @elves;

my $y = 0;
while ( <> ) {
    my $x = 0;
    for ( /[.#]/g ) {
        next unless /#/;
        my $elf = [$x, $y];
        push @elves, $elf;
    } continue { ++$x; }
    ++$y;
}

my %map;
$map{"@$_"} = 1 for @elves;

my $round = 0;
while ( round( $round++ ) ) {
    say solve_a() if $round == 10;
}

sub solve_a() {
    my @bounds = (0) x 4;
    for my $elf ( @elves ) {
        $bounds[0] = min $bounds[0], $elf->[0];
        $bounds[1] = min $bounds[1], $elf->[1];
        $bounds[2] = max $bounds[2], $elf->[0];
        $bounds[3] = max $bounds[3], $elf->[1];
    }

    return ($bounds[2] - $bounds[0] + 1) * ($bounds[3] - $bounds[1] + 1) - @elves;
}

say solve_a() if $round < 10;
say $round;

sub round($first_dir) {
    my %proposals;
    for my $elf ( @elves ) {
        next if check_around( $elf, \%map );
        for ( 0 .. 3 ) {
            my $dir = ( $first_dir + $_ ) % 4;
            next unless check_dir( $elf, $dir, \%map );
            my @next = step( $elf, $dir );
            push $proposals{"@next"}->@*, $elf;
            last;
        }
    }
    my $moved = 0;
    while ( my ($c, $es) = each %proposals ) {
        next unless @$es == 1;
        my @next = split $", $c;
        $map{"@{$es->[0]}"} = 0;
        $es->[0]->@* = @next;
        $moved = 1;
        $map{"@next"} = 1;
    }
    return $moved;
}

sub vecadd($a, $b) { map { $a->[$_] + $b->[$_] } 0, 1 }
sub step($e, $d) { vecadd( $e, $DIRS[$d] ) }
sub check_around ($e, $map) {
    check_dir( $e, 0, $map ) && check_dir( $e, 1, $map )
    && !$map->{join $", $e->[0] + 1, $e->[1]}
    && !$map->{join $", $e->[0] - 1, $e->[1]};
}
sub check_dir($e, $d, $map) {
    my @d = $DIRS[$d]->@*;
    my $axis = $d[0] == 0 ? 0 : 1;
    $d[$axis] = -1;
    for ( 1 .. 3 ) {
        my @c = map { $e->[$_] + $d[$_] } 0, 1;
        return 0 if $map->{"@c"};
        ++$d[$axis];
    }
    return 1;
}
