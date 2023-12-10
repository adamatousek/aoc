#!/usr/bin/env perl

use v5.36;
use strict;
use warnings;
use utf8;

use experimental qw/builtin for_list/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product/;

# north = 1, east = 2, south = 4, west = 8
my %CNX = ( '.' => 0b0000, '|' => 0b0101, '-' => 0b1010, 'S' => 0b1111,
            'L' => 0b0011, 'J' => 0b1001, 'F' => 0b0110, '7' => 0b1100 );

my @grid = map { chomp; [ split // ] } <>;
my @marks;

my ($sx, $sy) = find_start();

for ( 1, 2, 4, 8 )
{
    my $len = loop_len( $sx, $sy, $_ );
    if ( $len )
    {
        say $len / 2, " ($len)";
        last;
    }
}

my @counts = map flood_mark( $_ ), qw/l r/;
say $counts[ $marks[0][0] eq 'l' ];

##############################################################################

sub find_start() {
    for my $y ( 0 .. $#grid )
    {
        for my $x ( 0 .. $grid[$y]->$#* )
        {
            return $x, $y
                if $grid[$y][$x] eq 'S';
        }
    }
    die 'no start';
}

sub opposite( $d ) { ( $d >> 2 | $d << 2 ) & 0xf; }
sub dir2v( $d ) { ( !!( $d & 0b1010 ) * ( $d & 0b1001 ? -1 : 1 ),
                    !!( $d & 0b0101 ) * ( $d & 0b1001 ? -1 : 1 ) ); }
sub at( $x, $y )
{
    return 0 unless 0 <= $y < @grid && 0 <= $x <= $grid[$y]->@*;
    return $CNX{ $grid[$y][$x] };
}

sub loop_len( $x, $y, $d )
{
    my ($sx, $sy, $sd) = ($x, $y, $d);
    my $lastd;
    my $len = 0;

    do {
        mark( $x, $y, $d ) if $len;

        my ($dx, $dy) = dir2v( $d );
        $x += $dx;
        $y += $dy;
        my $neigh = at( $x, $y );
        return unless $neigh & opposite( $d ); # does not connect
        ++$len;
        $lastd = $d;
        $d = $neigh ^ opposite( $d );
    } until $x == $sx && $y == $sy;

    # replace S with pipe
    my $pipe = { reverse %CNX }->{ $sd | opposite( $lastd ) };
    $grid[$y][$x] = $pipe;

    mark( $x, $y, $sd );

    return $len;
}

sub mark( $x, $y, $d )
{
    $marks[ $y + 1 ][ $x + 1 ] = 'p';

    my $tile = at( $x, $y );
    my $dfrom = $tile ^ $d;
    my $fwd = $d > $dfrom;
    my $neighs = 0xf ^ $tile;
    my @sides = ( 'r', 'l' );
    for ( 1, 2, 4, 8 )
    {
        next unless $neighs & $_;

        my $left = min( $dfrom, $d ) < $_ < max( $dfrom, $d );
        my $side = $sides[ $left == $fwd ];

        my ($dx, $dy) = dir2v( $_ );
        my $n = \$marks[ 1 + $y + $dy ][ 1 + $x + $dx ];

        $side = 'X' if defined $$n && $$n ne $side; # possible if there is going to be 'p' later
        $$n = $side unless defined $$n && $$n eq 'p';
    }
}

sub flood_mark( $m ) {
    my $count = 0;
    my @q;
    for my $y ( 0 .. @grid )
    {
        for my $x ( 0 .. $grid[$y - 1]->@* )
        {
            my $old = $marks[ $y ][ $x ] // '';
            die if $old eq 'X'; # no conflicting marks
            next unless $m eq $old;
            ++$count;
            push @q, [ $x, $y ];
        }
    }

    while ( @q )
    {
        my $n = pop @q;
        for my $dx ( -1, 0, 1 )
        {
            for my $dy ( -1, 0, 1 )
            {
                my ($x, $y) = ( $n->[0] + $dx, $n->[1] + $dy );
                next unless 0 <= $x <= $grid[0]->@* && 0 <= $y <= @grid;
                next if defined $marks[ $y ][ $x ];
                $marks[ $y ][ $x ] = $m;
                push @q, [ $x, $y ];
                ++$count;
            }
        }
    }

    return $count;
}

sub print_grid() {
    for my $y ( 0 .. @grid )
    {
        for my $x ( 0 .. $grid[$y - 1]->@* )
        {
            my $mark = $marks[ $y ][ $x ] // ' ';
            $mark = $grid[ $y - 1 ][ $x - 1 ] if $mark eq 'p';
            print $mark;
        }
        print "\n";
    }
}
