#!/usr/bin/perl

use warnings;
use strict;

use Data::Dumper;
use List::Util qw/uniq all product/;

my $DIMS = 3;

my @coords;
push @coords, [] for 1 .. $DIMS;
my @areas;

LINE:
while ( <> ) {
    my $on = /^on /;
    my @cs = /-?\d+/g;
    my @res_cs;
    die if @cs != $DIMS * 2;
    for my $dim ( 0 .. $DIMS - 1 ) {
        my @c = splice @cs, 0, 2, ();
        @c = sort { $a <=> $b } @c;
        $c[1] += 1;
        push @res_cs, \@c;
        push $coords[ $dim ]->@*, @c;
    }
    push @areas, { on => $on // 0, coords => \@res_cs };
}

@coords = map [ uniq sort { $a <=> $b } @$_ ], @coords;

print "coords: ", scalar @$_, "\n" foreach @coords;

my $count_b = 0;
my $next_area_idx = 0;
for my $area ( @areas ) {
    ++$next_area_idx;
    next unless $area->{on};
    my $c = partition_and_go( $area, $next_area_idx, 0 );
    print "$c\n";
    $count_b += $c;
}

print "===================\n";
print "$count_b\n";

sub partition_and_go {
    my ($area, $i, $d, @acc ) = @_;
    if ( $d == $DIMS ) {
        return go( $i, @acc );
    }

    my $bounds = $area->{coords}[$d];
    my @cs = grep { $bounds->[0] <= $_ && $_ <= $bounds->[1] } $coords[$d]->@*;

    my $low = shift @cs;
    my $res = 0;
    foreach my $high ( @cs ) {
        $res += partition_and_go( $area, $i, $d + 1, @acc, [$low, $high] );
        $low = $high;
    }
    return $res;
}

sub go {
    my ($i, @cs) = @_;
    die unless @cs == $DIMS;
    for my $area ( @areas[ $i .. $#areas ] ) {
        if ( all { $cs[$_]->[0] >= $area->{coords}[$_][0]
                    && $cs[$_]->[1] <= $area->{coords}[$_][1] }
             ( 0 .. $DIMS - 1 ) )
        {
            return 0;
        }
    }
    return product map abs( $_->[0] - $_->[1] ), @cs;
}
