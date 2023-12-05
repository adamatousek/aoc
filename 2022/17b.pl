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

say (scalar @dirs);


my @rows;
for my $iterations ( 2022, 1_000_000_000_000 ) {
    my @memo;
    @rows = ([1,1,1,1,1,1,1]);
    my $top = 0;
    my $base = 0;
    my $steps = 0;

    my $forwarded = 0;
    for ( my $i = 0; $i < $iterations; ++$i ) {
        my $shape_def = $shapes[ $i % @shapes ];
        my $h = $shape_def->{h};
        my $w = $shape_def->{w};
        my $shape = $shape_def->{points};
        my $x = 2;
        my $y = $top + $h + 3;

        while ( 1 ) {
            my $wind = $dirs[ $steps ] eq '<' ? -1 : 1;
            $steps = ($steps + 1) % @dirs;
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
        if ( 7 == grep $_, $rows[$top]->@* ) {
            $base += $top;
            $top = 0;
            splice @rows, 1;

            next if $forwarded;
            say "i=$i: flat at height=$base";
            my $shape_i = $i % @shapes;

            my $m = \$memo[$shape_i][$steps];
            unless ( defined $$m ) {
                $$m = { top => $base, i => $i };
                next;
            }

            say "i=$i: height=$base; deja vu of s$shape_i at direction #$steps; from i0=$$m->{i} height=$$m->{top}";

            my $cycle_len = $i - $$m->{i};
            my $cycle_height = $base - $$m->{top};
            my $cycle_repeats = int( ( $iterations - $i - 1) / $cycle_len );
            $i += $cycle_repeats * $cycle_len;
            $base += $cycle_repeats * $cycle_height;

            say " * fast forward $cycle_repeats times to $i with height $base";
            $forwarded = 1;
        }
    }
    say $top + $base;
}

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
