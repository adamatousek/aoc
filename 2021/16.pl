#!/usr/bin/perl

use warnings;
use strict;
use List::Util qw/min max sum product/;
use Data::Dumper;

while ( <> ) {
    my $state = BitBuffer::new( $_ );
    my $tree = parse( $state );
    print versum( $tree ), " ", eval_pkt( $tree ), "\n";
}

sub versum {
    my $t = shift;
    my $sum = $t->{version};
    if ( $t->{subs} ) {
        $sum += versum( $_ )
            for $t->{subs}->@*;
    }
    return $sum;
}

sub eval_pkt {
    my $t = shift;
    return $t->{literal}
        if defined $t->{literal};
    my @s = map eval_pkt( $_ ), $t->{subs}->@*;
    for ( $t->{type} ) {
        return $_ == 0 ? sum( @s )
             : $_ == 1 ? product( @s )
             : $_ == 2 ? min( @s )
             : $_ == 3 ? max( @s )
             : $_ == 5 ? 0+( $s[0] > $s[1] )
             : $_ == 6 ? 0+( $s[0] < $s[1] )
             : $_ == 7 ? 0+( $s[0] == $s[1] )
             : die "Unknown packet type: $_";
    }
}

############################################################################
# Parsing

use constant PKT_LITERAL => 4;

sub parse {
    my $s = shift;
    my $version = $s->get( 3 );
    my $type    = $s->get( 3 );
    my %pkt = ( version => $version, type => $type );
    if ( $type == PKT_LITERAL ) {
        $pkt{literal} = parse_literal( $s );
    } else {
        my @subs;
        if ( $s->get( 1 ) ) {
            my $subs = $s->get( 11 );
            push @subs, parse( $s )
                for ( 1 .. $subs );
        } else {
            my $size = $s->get( 15 );
            my $target_bptr = $s->{bptr} + $size;
            push @subs, parse( $s )
                while ( $s->{bptr} < $target_bptr );
        }
        $pkt{subs} = \@subs;
    }
    return \%pkt;
}

sub parse_literal {
    my $s = shift;
    my $n = 0;
    my $last;
    do {
        $last = !$s->get( 1 );
        $n <<= 4;
        $n |= $s->get( 4 );
    } until $last;
    return $n;
}

############################################################################
# Bit buffer

package BitBuffer;

sub new {
    my $str = shift;
    chomp $str;
    return bless { src => $str, ptr => 0, buf => 0, blen => 0, bptr => 0 };
}

sub get {
    my ( $s, $nbits ) = @_;
    while ( $s->{blen} < $nbits ) {
        die "End of stream" if $s->{ptr} >= length $s->{src};
        my $nibble = substr $s->{src}, $s->{ptr}++, 1;
        $s->{blen} += 4;
        $s->{buf} <<= 4;
        $s->{buf} |= hex $nibble;
    }

    my $mask = ( 1 << $nbits ) - 1;
    my $res = ( $s->{buf} >> ( $s->{blen} - $nbits ) ) & $mask;
    $s->{blen} -= $nbits;
    $s->{bptr} += $nbits;
    return $res;
}
