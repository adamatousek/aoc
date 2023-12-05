#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/sum0 max/;

my %vs; # all valves, by two-letter id
my @vs; # valves with positive rat (and AA), by number

while ( <> ) {
    my ($id, @tunnels) = /[A-Z]{2}/g;
    my ($rate) = /\d+/g;
    my $v = {
        id => $id,
        rate => $rate,
        succ => \@tunnels,
    };
    $vs{$id} = $v;
    if ( $rate > 0 ) {
        push @vs, $v;
        $v->{num} = $#vs;
    }
}

bfs_flowing($_) for 'AA', map $_->{id}, @vs;

my $full_mask = ( 1 << @vs ) - 1;
unless ( defined $vs{AA}{num} ) {
    push @vs, $vs{AA};
    $vs{AA}{num} = $#vs;
}


my @memo;
sub solve_memo($time, $mask, $from, $cont_masks = undef) {
    return 0 if $mask == 0; # nothing to open, so just wait
    return 0 if $time <= 1; # nothing to do
    my $res = \$memo[$time][$from][$mask];

    return $$res if defined $$res;
    my $best = 0;
    for my $next ( mask2list($mask) ) {
        #die if $next > $#vs;
        #die if $from > $#vs;
        my $time_after_open = $time - $vs[$next]{dist}{$vs[$from]{id}} - 1;
        #die if $time_after_open >= $time;
        next if $time_after_open <= 0;
        my $next_mask = unmask($mask, $next);
        my $res = solve_memo($time_after_open, $next_mask, $next, $cont_masks);
        if ( $cont_masks and $res == 0 ) {
            $cont_masks->{$next_mask} = 1;
        }
        $res += $time_after_open * $vs[$next]{rate};
        $best = max $best, $res;
    }
    $$res = $best;
    return $best;
}
say 'part 1: ', solve_memo( 30, $full_mask, $#vs );

my %masks_for_elephant;
solve_memo( 26, $full_mask, $#vs, \%masks_for_elephant );

my $res_b = 0;
say 'interesting masks: ', scalar keys %masks_for_elephant;

for my $mask ( keys %masks_for_elephant ) {
    my $res = solve_memo( 26, $mask, $#vs )
            + solve_memo( 26, ~$mask & $full_mask, $#vs );
    $res_b = max $res_b, $res;
}
say 'part 2: ', $res_b;

exit;

# Dynamic programming!

sub mask2list($m) { grep { $m & (1 << $_) } 0 .. $#vs }
sub unmask($m, $i) { $m & ~( 1 << $i ) }

sub bfs_flowing($from) {
    my $dist = 0;
    my @doing = ( $vs{$from} );
    while ( @doing ) {
        my @todo;
        for my $v ( @doing ) {
            next if defined $v->{dist}{$from};
            $v->{dist}{$from} = $dist;
            push @todo, grep { not defined $_->{dist}{$from} } map $vs{$_}, $v->{succ}->@*;
        }
        $dist++;
        @doing = @todo;
    }
}
