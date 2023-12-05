#!/usr/bin/perl

use warnings;
use strict;

use List::Util qw/min max reduce/;

#my $init = '--BDDA-CCBD-BBAC-DACA--';
$init = '--DDDB-DCBC-BBAA-AACC--';
my $target = '--AAAA-BBBB-CCCC-DDDD--';
#$target = 'AABDDA-CCBD----CB--CABD';

my %ENERGY = ( A => 1, B => 10, C => 100, D => 1000 );

#############
#01.6.b.g.lm#
###2#7#c#h###
  #3#8#d#i#
  #4#9#e#j#
  #5#a#f#k#
  #########

my %DIST = (
    '0,1'   => 1,
    '1,2'   => 2,
    '1,6'   => 2,
    '2,6'   => 2,

    '2,3'   => 1,
    '3,4'   => 1,
    '4,5'   => 1,

    '6,7'   => 2,
    '6,11'  => 2,
    '7,11'  => 2,

    '7,8'   => 1,
    '8,9'   => 1,
    '9,10'  => 1,

    '11,12' => 2,
    '11,16' => 2,
    '12,16' => 2,

    '12,13'   => 1,
    '13,14'   => 1,
    '14,15'   => 1,

    '16,17'  => 2,
    '16,21' => 2,
    '17,21' => 2,

    '17,18' => 1,
    '18,19' => 1,
    '19,20' => 1,

    '21,22' => 1,
);

continue

my %FIRST = (
    A => 2,
    B => 7,
    C => 12,
    D => 17,
);

my %LAST = (
    A => 5,
    B => 10,
    C => 15,
    D => 20,
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
    my ($a, $b) = sort { $a <=> $b } @_;
    return $DIST{"$a,$b"};
}

sub successors {
    my @x = split //, $_[0];
    my @s;
    for my $i ( 0 .. 22 ) {
        my $me = $x[$i];
        next if $me eq '-';

        my $room = $HOME{$i};

        for my $jdiff ( 1, -1, 4, -4, 5, -5 ) {
            my $j = $i + $jdiff;
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
