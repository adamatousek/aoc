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

my $xmax = $grid[ 0 ]->$#*;
my $ymax = $#grid;

sub solve( $straight_min, $straight_max )
{
    my @dist;
    for my $y ( 0 .. $ymax )
    {
        $dist[ $y ][ $_ ] = [ 'inf', 'inf' ] for 0 .. $xmax;
    }
    $dist[ $ymax ][ $xmax ] = [ 0, 0 ];

    my $changed = 1;
    while ( $changed )
    {
        $changed = 0;
        for my $y ( 0 .. $ymax )
        {
            for my $x ( 0 .. $xmax )
            {
                for my $o ( 0, 1 )
                {
                    DIR:
                    for my $dir ( -1, 1 )
                    {
                        my $dx = $dir * $o;
                        my $dy = $dir * !$o;
                        my $loss = 0;
                        for my $dist ( 1 .. $straight_max )
                        {
                            my $nx = $x + $dx * $dist;
                            my $ny = $y + $dy * $dist;
                            next DIR unless 0 <= $nx <= $xmax
                                         && 0 <= $ny <= $ymax;

                            $loss += $grid[ $ny ][ $nx ];

                            next if $dist < $straight_min;

                            my $dist_r = \$dist[ $y ][ $x ][ $o ];
                            my $newdist = $dist[ $ny ][ $nx ][ !$o ] + $loss;
                            if ( $$dist_r > $newdist )
                            {
                                $$dist_r = $newdist;
                                $changed = 1;
                            }
                        }
                    }
                }
            }
        }
        last unless $changed;
    }

    return min $dist[ 0 ][ 0 ]->@*;
}

#print Dumper @dist;

say solve( 1, 3 );
say solve( 4, 10 );
