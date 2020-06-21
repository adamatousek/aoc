#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ min max any ];

my $DBG = 0;

sub iskey {
    return (shift @_) =~ /[a-z]/;
}
sub isdoor {
    return (shift @_) =~ /[A-Z]/;
}
sub keyof {
    return chr( ord(shift @_) + ord('a') - ord('A') );
}

my $INF = 9999999;

# "x,y" => { coord => "x,y", s => [\...], d => "a" }
my %maze;
my %clefs;

my $startpos;

my $x = 0;
my $y = 0;
while (my $line = <>) {
    foreach my $c ($line =~ /./g) {
        my $coord = "$x,$y";
        next if $c eq '#';
        $maze{$coord} = { coord => "$coord", s => [], d => $c };
        $startpos = $maze{$coord} if $c eq '@';
        $clefs{$c} = $maze{$coord} if iskey $c;
        my $towest = ($x - 1) . ",$y";
        my $tonorth = "$x," . ($y - 1);
        mutconnect($coord, $towest);
        mutconnect($coord, $tonorth);
    } continue {
        ++$x;
    }
    $x = 0;
    ++$y;
}

my %seen;
my $min_steps = $INF;
my @stack = ({ have => {}, position => $startpos, steps => 0 });

say 0+(keys %clefs);

while (my $st = pop @stack) {
    if ( (keys %{$st->{have}}) == (keys %clefs) ) {
        say "new min: $st->{steps}" if $st->{steps} < $min_steps;
        $min_steps = min $min_steps, $st->{steps};
        next;
    }
    next if ( $st->{steps} >= $min_steps );
    my $rks = reachable_keys( $st->{have}, $st->{position} );
    if ( $st->{steps} >= $rks->{steps} ) {
        next;
    } else {
        $rks->{steps} = $st->{steps};
    }
    my $rkeys = $rks->{rkeys};
    if ($DBG) {
        print "Being at $st->{position}{coord} after $st->{steps}, ";
        print "having ", (0+(keys %{$st->{have}})) ,"(", (keys %{$st->{have}}) ,") reachable are";
        print " $_" for keys %$rkeys;
        say "";
    }
    foreach my $k (keys %$rkeys) {
        my $newkeys = {%{$st->{have}}};
        $newkeys->{"$k"} = 1;
        push @stack, { have => $newkeys, position => $clefs{"$k"}, steps => $st->{steps} + $rkeys->{"$k"} };
    }
}

say "Min steps: $min_steps";


sub mutconnect {
    my ($u, $v) = @_;
    my $ru = $maze{$u};
    my $rv = $maze{$v};
    return unless defined $ru and defined $rv;
    push @{$ru->{s}}, $rv;
    push @{$rv->{s}}, $ru;
}

sub reachable_keys {
    my ($have, $from) = @_;
    my $hk = hash_keys($have);
    my $h = "$from->{coord}:$hk";
    my $st = $seen{"$h"};
    return $st if defined $st;
    my $s = { rkeys => do_reachable_keys($have, $from), steps => $INF };
    say "New state $h" if $DBG;
    say Dumper $s->{rkeys} if $DBG;
    $seen{"$h"} = $s;
    return $s;
}

sub do_reachable_keys {
    my ($have, $from) = @_;
    say "BFS from $from->{coord}, having ", %$have if $DBG;
    my $found = {};
    my $vis = { "$from->{coord}" => 0 };
    my @q = ($from);
    while (my $v = shift @q) {
        my $c = $v->{d};
        my $w = $vis->{"$v->{coord}"} ;
        $found->{"$c"} = $w if (iskey $c and not $have->{"$c"});
        if ( isdoor $c ) {
            my $kod = keyof $c;
            next unless defined $have->{"$kod"};
        }
        my $nextw = $vis->{"$v->{coord}"} + 1;
        foreach my $s (@{$v->{s}}) {
            next if defined $vis->{"$s->{coord}"};
            push @q, $s;
            $vis->{"$s->{coord}"} = $nextw;
        }
    }
    return $found;
}

sub hash_keys {
    my $ks = shift;
    my $s = 0;
    foreach my $k (keys %$ks) {
        $s += 1 << (ord("$k") - ord('a'));
    }
    return $s;
}
