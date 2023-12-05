#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my @dirs = <> =~ /[<>]/g;
my @shapes = (
    [[1,1,1,1]],

    [[0,1,0]
    ,[1,1,1]
    ,[0,1,0]],

    [[0,0,1]
    ,[0,0,1]
    ,[1,1,1]],

    [[1],[1],[1],[1]],

    [[1,1]
    ,[1,1]]
);
@shapes = map {
        my $s = $_;
        my %s;
        for my $y ( 0 .. $s->$#* ) {
            $s{"$_;$y"} = $s->[$y][$_] for 0 .. $s->[$y]->$#*;
        }
        { points => \%s, h => scalar @$s, w => scalar $s->[0]->@* };
    } @shapes;

my $top = 0;
my @rows = ([1,1,1,1,1,1,1]);

my $steps = 0;
for my $i ( 0 .. 2021 ) {
    my $shape_def = $shapes[ $i % @shapes ];
    my $h = $shape_def->{h};
    my $w = $shape_def->{w};
    my $shape = $shape_def->{points};
    my $x = 2;
    my $y = $top + $h + 3;

    while ( 1 ) {
        my $wind = $dirs[ $steps++ % @dirs ] eq '<' ? -1 : 1;
        $x += $wind unless collides( $shape_def, $x + $wind, $y );
        last if collides( $shape_def, $x, $y - 1 );
        --$y;
    }

    #settle
    for ( keys %$shape ) {
        next unless $shape->{$_};
        my @c = split ';';
        $rows[$y - $c[1]][$x + $c[0]] = 1;
    }
    $top = max $top, $y;
}

# 3497 too high
say $top;
exit;

sub collides($shape, $x, $y) {
    return 1 if $x < 0 || $y <= 0 || $x + $shape->{w} > 7 || $y - $shape->{h} < 0;
    for ( keys $shape->{points}->%* ) {
        next unless $shape->{points}{$_};
        my @c = split ';';
        return 1 if $rows[$y - $c[1]][$x + $c[0]];
    }
    return 0;
}

sub draw() {
    for my $r ( 0 .. $#rows ) {
        say ( join '', map { $_ ? '#' : '.' } $rows[$#rows - $r]->@* );
    }
}
