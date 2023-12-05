#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/sum0 max/;

my %vs;
my %flowing;
my @vs;
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
        $flowing{$id} = $rate;
        push @vs, $v;
        $v->{num} = $#vs;
    }
}

bfs_flowing($_) for keys %flowing, 'AA';

print Dumper \%vs, \%flowing;
print Dumper \@vs;


# parametry (pod)problému: zbývající čas    (1 .. 30)
#                          zavřené ventily  (2^15)
#                          výchozí posice   (1..15)
# výsledek podproblému: úhrn nově uvolněného tlaku za celý dostupný čas

my @solution;
sub solution($time, $mask, $from) {
    my $s = $solution[$mask][$time][$from];
    #die "Nothing at time $time, from $from with mask $mask!" unless defined $s;
    return $s;
}

for my $time ( 0 .. 30 ) {
    for my $from ( 0 .. $#vs ) {
        for my $mask ( 0 .. ( 1 << @vs ) - 1 ) {
            # print STDERR "** $time, $from, $mask ";
            $solution[$mask][$time][$from] = solve( $time, $mask, $from );
            # print STDERR "= $solution[$mask][$time][$from]\n";
        }
        say "* done from $from";
    }
    say "*** done time $time";
}

sub solve($time, $mask, $from) {
    return 0 if $mask == 0; # nothing to open, so just wait
    return 0 if $time == 0; # nothing to do
    my $best = 0;
    for my $next ( mask2list($mask) ) {
        #die if $next > $#vs;
        #die if $from > $#vs;
        my $time_after_open = $time - $vs[$next]{dist}{$vs[$from]{id}} - 1;
        #die if $time_after_open >= $time;
        next if $time_after_open <= 0;
        my $res = solution($time_after_open, unmask($mask, $next), $next);
        $res += $time_after_open * $vs[$next]{rate};
        $best = max $best, $res;
    }
    return $best;
}

my $res_a = 0;
for my $v ( @vs ) {
    my $time_after_arrival = 30 - $v->{dist}{'AA'};
    next if $time_after_arrival <= 0;
    my $res = solution($time_after_arrival, full_mask( scalar @vs ), $v->{num});
    $res_a = max $res_a, $res;
}
say $res_a;


exit;

sub value($from, $to, $time) { ( $time - $vs{$from}{dist}{$to} - 1 ) * $vs{$to}{rate} }
sub mask2list($m) { grep { $m & (1 << $_) } 0 .. 15 }
sub unmask($m, $i) { $m & ~( 1 << $i ) }
sub full_mask($n) { ( 1 << $n ) - 1 };

sub bfs_flowing($from) {
    my $dist = 0;
    my @doing = ($vs{$from});
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
