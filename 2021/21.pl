#!/usr/bin/perl

use warnings;
use strict;
use List::Util qw/min max/;

my @score = ( 0, 0 );
my @input = map /(\d)$/, <>;

# Part A
my @pos = @input;
my $player = 0;
my $rolls = 0;
my $die = 0;

() until step();
print $score[ $player ] * $rolls, "\n";

sub step {
    my $roll = 0;
    $roll += roll() for 1 .. 3;
    $pos[ $player ] += $roll;
    $pos[ $player ] %= 10;
    $score[ $player ] += $pos[ $player ] || 10;
    my $won = $score[ $player ] >= 1000;
    $player = !$player;
    return $won;
}

sub roll {
    ++$rolls;
    ++$die;
    $die = 1 if $die > 100;
    return $die;
}


# Part B

my %dyn;

my $score_sum = 41;
while ( $score_sum --> 0 ) {
    my $s2 = 1 + min $score_sum, 20;
    while ( $s2 --> 0 ) {
        my $s1 = $score_sum - $s2;
        next if $s1 < 0 || $s1 >= 22;
        for my $p1 ( 0 .. 9 ) {
            for my $p2 ( 0 .. 9 ) {
                my $wins = 0;
                my $loses = 0;
                for ( [ 1, 3 ], [ 3, 4 ], [ 6, 5 ], [ 7, 6 ], [ 6, 7 ], [ 3, 8 ], [ 1, 9 ] ) {
                    my ( $worlds, $roll ) = @$_;
                    my $p1new = ( $p1 + $roll ) % 10;
                    my $s1new = $s1 + ( $p1new || 10 );
                    my ( $theirs, $ours ) = map { $_ * $worlds } load( $p2, $s2, $p1new, $s1new );
                    $wins += $ours;
                    $loses += $theirs;
                }

                store( $p1, $s1, $p2, $s2, $wins, $loses );
            }
        }
    }
}

my @res = load( $input[0], 0, $input[1], 0 );
my $m = max @res;
print join ' ', load( $input[1], 0, $input[0], 0 ), "\n";
print "@res -> max = $m\n";

sub store {
    my ( $p1, $s1, $p2, $s2, $p1w, $p2w ) = @_;

    my $k = join ';', @_[ 0 .. 3 ];
    my $old = $dyn{$k};
    if ( defined $old and $old->[0] != $p1w || $old->[1] != $p2w ) {
        die "$k was @$old, is $p1w $p2w";
    }
    $dyn{$k} = [ @_[ 4, 5 ] ];
}

sub load {
    my ( $p1, $s1, $p2, $s2 ) = @_;
    die 'two winners' if $_[1] >= 21 && $_[0] >= 21;
    return (1, 0) if $_[1] >= 21;
    return (0, 1) if $_[3] >= 21;
    my $k = join ';', @_[ 0 .. 3 ];
    defined $dyn{$k} or die "not pre-computed: $k";
    return $dyn{$k}->@*;
}
