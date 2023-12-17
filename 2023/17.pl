#!/usr/bin/env perl

use v5.36;

package Node;

sub new( $self, $x, $y, $o ) { bless [ $x, $y, $o, 'inf' ] }
sub cmp( $self, $o ) { $self->[3] <=> $o->[3] }
sub heap( $self, $h = undef ) { $self->[4] = $h if defined $h; return $self->[4]; }
sub dist( $self, $d = undef ) { $self->[3] = $d if defined $d; return $self->[3]; }

package main;

use experimental qw/builtin for_list/;
use builtin qw/indexed/;

use Data::Dumper;
$Data::Dumper::Sortkeys = 1;

use List::Util qw/min max sum product/;
use Heap::Binary;
use Heap::Elem::Ref qw/RefElem/;

my @grid = map { chomp; [ split // ] } <>;
my $xmax = $grid[ 0 ]->$#*;
my $ymax = $#grid;

sub solve( $straight_min, $straight_max )
{
    my @nodes;
    for my $y ( 0 .. $ymax )
    {
        for my $x ( 0 .. $xmax )
        {
            $nodes[ $y ][ $x ][ $_ ] = Node->new( $x, $y, $_ ) for 0, 1;
        }
    }
    $nodes[0][0][ $_ ]->dist( 0 ) for 0, 1;

    my $heap = Heap::Binary->new;
    $heap->add( $nodes[0][0][ $_ ] ) for 0, 1;

    while ( defined( my $node = $heap->extract_top ) )
    {
        my ( $x, $y, $o, $d ) = @$node;

        return $d if $x == $xmax && $y == $ymax;

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

                my $newdist = $d + $loss;
                my $nnode = $nodes[ $ny ][ $nx ][ !$o ];
                if ( $nnode->dist > $newdist )
                {
                    $nnode->dist( $newdist );
                    $heap->add( $nnode );
                }
            }
        }
    }

    return min $nodes[0][0][0]->dist, $nodes[0][0][1]->dist;
}

say solve( 1, 3 );
say solve( 4, 10 );
