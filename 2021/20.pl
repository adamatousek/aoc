#!/usr/bin/perl

use warnings;
use strict;

my @rules = map { $_ eq '#' } <> =~ /./g;
my @image;
my $fill = 0;

<>;
while ( <> ) {
    push @image, [ map { $_ eq '#' } /./g ];
}

step for 1 .. 2;
print hlcount, "\n";
step for 3 .. 50;
print hlcount, "\n";

sub step {
    frame;
    enhance;
    $fill = $rules[ $fill * 0x1FF ];
}

sub frame {
    my $w = $image[0]->@*;
    unshift @image, [ ($fill) x $w ];
    push @image, [ ($fill) x $w ];
    for my $row ( @image ) {
        unshift @$row, $fill;
        push @$row, $fill;
    }
}

sub enhance {
    my @new;
    for my $y ( 0 .. $#image ) {
        my @newrow;
        push @newrow, map enhance_one( $_, $y ), 0 .. $image[$y]->$#*;
        push @new, \@newrow;
    }
    @image = @new;
}

sub enhance_one {
    my ($x, $y) = @_;
    my $bits = 0;
    for my $dy ( -1 .. 1 ) {
        for my $dx ( -1 .. 1 ) {
            $bits <<= 1;
            $bits |= at( $x + $dx, $y + $dy );
        }
    }
    return $rules[ $bits ]
}

sub at {
    my ($x, $y) = @_;
    return ( $x < 0 || $x > $image[0]->$#* || $y < 0 || $y > $#image )
        ? $fill : $image[$y][$x];
}

sub hlcount {
    my $c = 0;
    for my $r ( @image ) {
        $c += grep { $_ } @$r;
    }
    $c .= '+inf' if $fill;
    return $c;
}
