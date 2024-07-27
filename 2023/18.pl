#!/usr/bin/env perl

use v5.36;

use experimental qw/builtin for_list/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product/;

my %map;
my ($x, $y) = (0, 0);
my ( $xmin, $ymin, $xmax, $ymax ) = ( 'inf', 'inf', '-inf', '-inf' );

my $sol_a = 0;

while ( <> )
{
    my ( $dir, $len, $colour ) = /(.) (\d+) \(#(.{6})\)/;
    my ( $dx, $dy ) = dir2vec( $dir );
    for ( 1 .. $len )
    {
        $x += $dx;
        $y += $dy;
        $map{ "$x $y" } = $colour;
        ++ $sol_a;
        $xmin = min $xmin, $x;
        $ymin = min $ymin, $y;
        $xmax = max $xmax, $x;
        $ymax = max $ymax, $y;
    }
}

say "$xmin $ymin, $xmax $ymax";

ROW:
for my $y ( $ymin + 1 .. $ymax - 1 )
{
    for my $x ( $xmin .. $xmax )
    {
        if ( defined $map{ "$x $y" } )
        {
            my $nx = $x + 1;
            if ( defined $map{ "$nx $y" } )
            {
                next ROW;
            }
            else
            {
                $sol_a += fill( $nx, $y );
                last ROW;
            }
        }
    }
}

say $sol_a;


sub dir2vec( $d )
{
    return  1,  0 if $d eq 'R';
    return -1,  0 if $d eq 'L';
    return  0,  1 if $d eq 'D';
    return  0, -1 if $d eq 'U';
    die;
}

sub fill( $x, $y )
{
    my @todo = ( $x, $y );
    my $filled = 0;
    while ( @todo )
    {
        my ( $x, $y ) = splice @todo, -2;
        foreach ( qw/R L D U/ )
        {
            my ( $dx, $dy ) = dir2vec( $_ );
            my $coords = ( $x + $dx ) . ' ' . ( $y + $dy );
            next if defined $map{ $coords };
            $map{ $coords } = 0;
            ++ $filled;
            push @todo, $x + $dx, $y + $dy;
        }
    }
    return $filled;
}
