#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ min max any ];

my $DBG = 0;

my $INF = 9999999;

sub isupper {
    return (shift @_) =~ /[A-Z]/;
}

my %maze;
my %portparts;
my %ports;

my $startpos;

my $x = 0;
my $y = 0;
my $line = <>;
my $width = (length $line) - 1; # do not count newline
my @maze_up;
do {
    my $was_maze = 0;
    foreach my $c ($line =~ /./g) {
        my $coord = "$x,$y";
        my $towest = ($x - 1) . ",$y";
        my $tonorth = "$x," . ($y - 1);
        if (isupper $c) {
            my $pred;
            my $dir;
            $dir = 'h' if $pred = delete $portparts{$towest};
            $dir = 'v' if !$pred and $pred = delete $portparts{$tonorth};
            if (defined $pred) {
                my $portname = $pred . $c;
                my $d;
                if ($dir eq 'h') {
                    $d = ($was_maze ? -2 : 1);
                } else {
                    $d = ($maze_up[$x] ? -2 : 1);
                }
                my $portx = $dir eq 'h' ? $x + $d : $x;
                my $porty = $dir eq 'v' ? $y + $d : $y;
                push @{$ports{$portname}}, "$portx,$porty";
            } else {
                $portparts{$coord} = $c;
            }
        }
        unless (isupper $c) {
            $was_maze = ($c eq '#' || $c eq '.');
            $maze_up[$x] = $was_maze;
        }
        next unless $c eq '.';
        $maze{$coord} = { coord => "$coord", s => [] };
        mutconnect($coord, $towest);
        mutconnect($coord, $tonorth);
    } continue {
        ++$x;
    }
    $x = 0;
    ++$y;
} while ($line = <>);
my $height = $y;

my ($startc, $endc) = ("0:" . $ports{AA}->[0], "0:" . $ports{ZZ}->[0]);
delete %ports{AA};
delete %ports{ZZ};

print Dumper \%ports;

foreach my $portn (keys %ports) {
    my ($uc, $vc) = @{$ports{$portn}};
    my $u = $maze{$uc};
    my $v = $maze{$vc};
    say "Connecting port $portn = $uc <=> $vc";
    die unless defined $u and defined $v;
    ($u, $v) = ($v, $u) if outer($uc);
    $u->{enter} = $v;
    $v->{leave} = $u;
}

say "Width: $width, height: $height";
say "BFS from $startc to $endc";

sub bfs {
    my $vis = { $startc => 0 };
    my @q = ($startc);
    while (my $vc = shift @q) {
        my ($lvl,$coord) = $vc =~ /(\d+):(.*)/;
        my $v = $maze{$coord};
        #say "Processing $vc (= lvl $lvl, coords $coord)";
        my $w = $vis->{$vc};
        my $nextw = $w + 1;
        foreach my $s (@{$v->{s}}) {
            my $sc = "$lvl:$s->{coord}";
            next if defined $vis->{$sc};
            return $nextw if $sc eq $endc;
            push @q, $sc;
            $vis->{$sc} = $nextw;
        }
        if (my $entv = $v->{enter}) {
            my $entc = ($lvl + 1) . ":$entv->{coord}";
            push @q, $entc;
            $vis->{$entc} = $nextw;
        }
        if (my $leav = $v->{leave} and $lvl > 0) {
            my $leac = ($lvl - 1) . ":$leav->{coord}";
            push @q, $leac;
            $vis->{$leac} = $nextw;
        }
    }
}

my $steps = bfs;
say "Steps: $steps";

sub outer {
    my ($x,$y) = $_[0] =~ /^(\d+),(\d+)$/;
    return $x == 2 || $y == 2 || $x == $width - 3 || $y == $height - 3;
}

sub mutconnect {
    my ($u, $v) = @_;
    my $ru = $maze{$u};
    my $rv = $maze{$v};
    return 0 unless defined $ru and defined $rv;
    push @{$ru->{s}}, $rv;
    push @{$rv->{s}}, $ru;
    return 1;
}
