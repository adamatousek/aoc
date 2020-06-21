#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ min max any sum ];
use Math::BigInt;
use integer;

my $PARTB = 1;

my $deckl = $PARTB ? 119315717514047 : 10007;
my $cardmax = $deckl - 1;
my $card = $PARTB ? 2020 : 2322;
my $c = $card; # Position of card

my $lin = 1;
my $con = 0;

my @input;
push @input, $_ for <>;
@input = reverse @input if $PARTB;

foreach my $cmd (@input) {
    given ($cmd) {
        deal_onto_new_B() when /deal into new stack/;
        cut_n_B($1)       when /cut (-?\d+)/;
        deal_with_B($1)   when /deal with increment (\d+)/;
        default { die "Unknown command: $cmd" }
    }
}

my %aps = ( 0 => { lin => $lin, con => $con } );
say Dumper $aps{0};
#my $foo = Math::BigInt->new( 2322 );
#$foo->bmul( $aps{0}->{lin} );
#$foo->badd( $aps{0}->{con} );
#$foo->bmod( $deckl );
#say $foo;
#
#exit;

my $fun = { lin => $lin, con => $con };
my $i = 101741582076661 >> 1;
my $j = 0;
while ( $i > 0 ) {
    my $f_pre = $aps{$j++};
    my $f = compose( $f_pre, $f_pre );
    $aps{$j} = $f;
    $fun = compose( $f, $fun ) if ( $i & 1 );
    $i >>= 1;
}

print "Result: ";
my $res = Math::BigInt->new( 2020 );
$res->bmul( $fun->{lin} );
$res->badd( $fun->{con} );
$res->bmod( $deckl );
say $res;

sub compose {
    my ($f, $g) = @_;
    my $lin = Math::BigInt->new( $f->{lin} );
    my $con = $lin->copy();
    $lin->bmul( $g->{lin} );
    $lin->bmod( $deckl );
    $con->bmul( $g->{con} );
    $con->badd( $f->{con} );
    $con->bmod( $deckl );
    return { lin => $lin->numify(), con => $con->numify() };
}

sub deal_onto_new_B {
    if ($PARTB) {
        $con = $deckl + $deckl - $con - 1;
        $lin = $deckl + $deckl - $lin;
        $con %= $deckl;
        $lin %= $deckl;
    } else {
        $c = $cardmax - $c;
    }
}

sub cut_n_B {
    my $n = shift;
    if ( $PARTB ) {
        $con += $deckl + $n;
        $con %= $deckl;
    } else {
        if ($n < 0) {
            if ( $c < $deckl + $n ) {
                $c -= $n;
            } else {
                $c = (-$n) - ($deckl - $c);
            }
        } else {
            if ( $c >= $n ) {
                $c -= $n;
            } else {
                $c = $deckl - ($n - $c);
            }
        }
    }
}

my %inversions;

sub deal_with_B {
    my $n = shift;
    if ($PARTB) {
        unless (defined $inversions{$n}) {
            my $inv = Math::BigInt->new( $n );
            $inv->bmodinv( $deckl );
            $inversions{$n} = $inv;
        }
        my $newlin = $inversions{$n}->copy();
        my $newcon = $newlin->copy();
        $newlin->bmul( $lin );
        $newlin->bmod( $deckl );
        $newcon->bmul( $con );
        $newcon->bmod( $deckl );
        $lin = $newlin->numify();
        $con = $newcon->numify();
        return;
    } else {
        # $c = ($c * $n) % $deckl;
        --$n;
        my $oldc = $c;
        while ( $n-- ) {
            $c += $oldc;
            $c %= $deckl;
        }
    }
}

#sub deal_onto_new {
#    @deck = reverse @deck;
#}
#
#sub cut_n {
#    my $n = shift;
#    my @tmp;
#    if ($n < 0) {
#        @tmp = splice @deck, $n;
#        splice @deck, 0, 0, @tmp;
#        @tmp == -$n or die;
#    } else {
#        @tmp = splice @deck, 0, $n;
#        splice @deck, @deck, 0, @tmp;
#        @tmp == $n or die;
#    }
#}
#
#sub deal_with {
#    my $n = shift;
#    my @tmp;
#    for (my $i = 0; $i < @deck; $i++) {
#        my $j = ($i * $n) % @deck;
#        die "Overwriting position $j with index $i" if defined $tmp[$j];
#        $tmp[$j] = $deck[$i];
#    }
#    @deck = @tmp;
#}
