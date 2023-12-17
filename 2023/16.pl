#!/usr/bin/env perl

use v5.36;

use experimental qw/builtin for_list/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product/;

my @grid;
while ( <> )
{
    chomp;
    push @grid, [ split // ];
}

my $width = $grid[ 0 ]->@*;
my $height = @grid;

say solve( 0, 0, 1, 0 );

my $sol_b = max map { solve( 0, $_, 1, 0 ), solve( $width - 1, $_, -1, 0 ) } 0 .. $height - 1;
$sol_b = max $sol_b, map { solve( $_, 0, 0, 1 ), solve( $_, $height - 1, 0, -1 ) } 0 .. $width - 1;
say $sol_b;

sub solve( $sx, $sy, $sdx, $sdy )
{
    my @vis;
    $vis[ $sy ][ $sx ] = dir2mask( $sdx, $sdy );
    my @q = ( [ $sx, $sy, $sdx, $sdy ] );

    while ( @q )
    {
        my ( $x, $y, $dx, $dy ) = @{ shift @q };
        my $tile = $grid[ $y ][ $x ];
        my @next_dirs;
        if ( $tile eq '.'
            || ( $tile eq '-' && $dy == 0 )
            || ( $tile eq '|' && $dx == 0 ) )
        {
            push @next_dirs, $dx, $dy;
        }
        else
        {
            push @next_dirs, $dy, $dx if $tile =~ m,[-|\\],;
            push @next_dirs, -$dy, -$dx if $tile =~ m,[-|/],;
        }

        for my ( $ndx, $ndy ) ( @next_dirs )
        {
            my $nx = $x + $ndx;
            my $ny = $y + $ndy;
            next unless 0 <= $nx < $width
                     && 0 <= $ny < $height;
            my $seen = $vis[ $ny ][ $nx ] // 0;
            my $seen_dir = dir2mask( $ndx, $ndy );

            $vis[ $ny ][ $nx ] |= $seen_dir;

            next if $seen & $seen_dir && $tile =~ /[-|]/;
            push @q, [ $nx, $ny, $ndx, $ndy ];
        }
    }
    return sum map { scalar grep $_, @$_ } @vis;
}

sub dir2mask( $dx, $dy ) { $dx ? ( $dx == 1 ? 1 : 2 ) : ( $dy == 1 ? 4 : 8 ) }
