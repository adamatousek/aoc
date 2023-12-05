#!/usr/bin/perl

use warnings;
use strict;

use List::Util qw/min max reduce/;

my $init = '--BA-CD-BC-DA--';
$init = '--DB-DC-BA-AC--';
my $target = '--AA-BB-CC-DD--';
# $target = '---A-BB-CC-DDA-';

my %ENERGY = ( A => 1, B => 10, C => 100, D => 1000 );

#############
#01.4.7.a.de#
###2#5#8#b###
  #3#6#9#c#
  #########

my %DIST = (
    '0,1'   => 1,
    '1,2'   => 2,
    '1,4'   => 2,
    '2,3'   => 1,
    '2,4'   => 2,
    '4,5'   => 2,
    '4,7'   => 2,
    '5,6'   => 1,
    '5,7'   => 2,
    '7,8'   => 2,
    '7,10'  => 2,
    '8,9'   => 1,
    '8,10'  => 2,
    '10,11' => 2,
    '10,13' => 2,
    '11,12' => 1,
    '11,13' => 2,
    '13,14' => 1,
);

my %FIRST = (
    A => 2,
    B => 5,
    C => 8,
    D => 11,
);

my %LAST = (
    A => 3,
    B => 6,
    C => 9,
    D => 12,
);

my %HOME = map { my $k = $_; map { $_ => $k } ( $FIRST{$k} .. $LAST{$k} ) } keys %FIRST;

my %states = ( $init => { state => $init, cost => 0 } );
my %q = ( $init => 1 );
my @q = ( $init );

while ( @q ) {
    # my $st = reduce { $states{$a}{cost} < $states{$b}{cost} ? $a : $b } keys %q;
    # delete $q{$st};
    my $st = shift @q;

    my $state = $states{$st};
    $state->{active} = 0;

    if ( $st eq $target ) {
        print "$state->{cost}\n";
        exit;
    }
    for my $n ( successors( $st ) ) {
        my $cost = $n->{cost} + $state->{cost};
        if ( ! $states{$n->{state}} || $states{$n->{state}}{cost} > $cost ) {
            $states{$n->{state}}{cost} = $cost;
            $states{$n->{state}}{state} = $n->{state};
            # $q{$n->{state}} = 1;
            push @q, $n->{state} unless $states{$n->{state}}{active};
            $states{$n->{state}}{active} = 1;
        }
    }
}
die "no such state";

sub dist {
    my ($a, $b) = @_;
    return $a < $b ? $DIST{"$a,$b"} : $DIST{"$b,$a"};
}

sub successors {
    my @x = split //, $_[0];
    my @s;
    for my $i ( 0 .. 14 ) {
        my $me = $x[$i];
        next if $me eq '-';

        my $room = $HOME{$i};

        for my $j ( max( 0, $i - 3 ) .. min( 14, $i + 3 ) ) {
            my $d = dist( $i, $j );
            next unless $d;
            next unless $x[$j] eq '-';

            if ( $room ) { # i am in a room
                if ( $room eq $me ) { # my room
                    my $room_ok = 1;
                    for ( $i + 1 .. $LAST{$me} ) {
                        $room_ok &&= $x[$_] eq '-' || $x[$_] eq $me;
                    }
                    next if ( $j == $i + 1 ) == !$room_ok; # don't go back if there is no foreigner, don't continue if there is one
                }
                else { # not my room
                    next if $j == $i + 1; # don't go further
                }
            }
            else { # outside a room
                next if $me ne ( $HOME{$j} // $me ); # don't enter foreign rooms
                if ( $j == $FIRST{$me} ) { # about to enter my room
                    my $room_ok = 1;
                    for ( $j .. $LAST{$me} ) {
                        $room_ok &&= $x[$_] eq '-' || $x[$_] eq $me;
                    }
                    next if !$room_ok; # a foreigner is there
                }
            }

            my @y = @x;
            $y[$j] = $me;
            $y[$i] = '-';
            push @s, { state => (join '', @y), cost => $d * $ENERGY{$me} };
        }
    }
    return @s;
}
