#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max product/;

my @bliz;
my %DIRS = ( '^' => [0, -1], 'v' => [0, 1], '<' => [-1, 0], '>' => [1, 0] );

<>; # ignore first line
my $y = 0;
my @size = (0, 0);
while ( <> ) {
    my $x = -1;
    for ( /./g ) {
        next unless /[<>^v]/;
        my $bliz = { loc => [$x, $y], dir => $DIRS{$_} };
        push @bliz, $bliz;
    } continue { ++$x; }
    $size[0] = $x - 1;
    ++$y;
}
$size[1] = $y - 1;

# materialise blizards at each possible state
my @grids;
for ( 1 .. product @size ) {
    my @grid;
    for my $bliz ( @bliz ) {
        $grid[$bliz->{loc}[1]][$bliz->{loc}[0]] = 1;
        $bliz->{loc}[$_] += $bliz->{dir}[$_] for 0, 1;
        $bliz->{loc}[$_] %= $size[$_] for 0, 1;
    }
    push @grids, \@grid;
}

my %vis;
sub free($x, $y, $t) {
    return 1 if $x == 0 && $y == -1; # start
    return 1 if $x == $size[0] - 1 && $y == $size[1]; # end
    return 0 unless 0 <= $x < $size[0];
    return 0 unless 0 <= $y < $size[1];
    return !$grids[$t % @grids][$y][$x] && !$vis{"$x $y $t"};
}

my @q = ( [0, -1, 0] );
$vis{"@$_"} = 1 for @q;
my $dist = 0;

my @targets = ( [$size[0] - 1, $size[1]], [0, -1], [$size[0] - 1, $size[1]] );
while ( @q ) {
    my @todo;
    CURRENT_QUEUE:
    for ( @q ) {
        my ($x, $y, $t) = @$_;
        my $t2 = ( $t + 1 ) % @grids;
        for my $d ( [0, 0], values %DIRS ) {
            my ($x2, $y2) = ( $x + $d->[0], $y + $d->[1] );
            next unless free( $x2, $y2, $t2 );
            $vis{"$x2 $y2 $t2"} = 1;
            push @todo, [$x2, $y2, $t2];

            if ( $x2 == $targets[-1][0] && $y2 == $targets[-1][1] ) {
                say $dist + 1 if @targets % 2 == 1;
                @todo = ( [$x2, $y2, $t2] );
                %vis = ( "$x2 $y2 $t2" => 1 );
                pop @targets;
                exit unless @targets;
                last CURRENT_QUEUE;
            }
        }
    };
    ++$dist;
    @q = @todo;
}
