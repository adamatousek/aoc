#!/usr/bin/perl

use warnings;
use strict;

my %cave;
my $dest;
my $y = 0;
while ( <> ) {
    my @cs = /\d/g;
    while ( my ($x, $lvl) = each @cs ) {
        $dest = "$x,$y";
        $cave{$dest} = { lvl => $lvl, cost => 'inf' };
    }
    ++$y;
}
my $dest_a = $dest;

my ($mx, $my) = split /,/, $dest;
for my $sx ( 0 .. 4 ) {
    for my $sy ( 0 .. 4 ) {
        next if !$sx && !$sy;
        for my $dx ( 0 .. $mx ) {
            for my $dy ( 0 .. $my ) {
                my $x = $sx * ($mx + 1) + $dx;
                my $y = $sy * ($my + 1) + $dy;
                my $lvl = $cave{"$dx,$dy"}{lvl} + $sx + $sy;
                $lvl -= 9 if $lvl > 9;
                $dest = "$x,$y";
                $cave{$dest} = { lvl => $lvl, cost => 'inf' };
            }
        }
    }
}
my $dest_b = $dest;

$cave{"0,0"}{cost} = 0;
my @q = ( "0,0" );

while ( @q ) {
    my $n = shift @q;
    $cave{$n}{enqueued} = 0;
    my $cost = $cave{$n}{cost};
    for ( neighs( $n ) ) {
        my $ncost = $cost + $cave{$_}{lvl};
        if ( $ncost < $cave{$_}{cost} ) {
            $cave{$_}{cost} = $ncost;
            push @q, $_ unless $cave{$_}{enqueued};
            $cave{$_}{enqueued} = 1;
        }
    }
}

print "$cave{$_}{cost}\n" for ( $dest_a, $dest_b );

sub neighs {
    my ($x, $y) = split /,/, $_[0];
    my @ns;
    for my $dx ( -1 .. 1 ) {
        for my $dy ( -1 .. 1 ) {
            my $n = join ',', ( $dx + $x, $dy + $y );
            next unless abs($dy) + abs ($dx) == 1;
            push @ns, $n if exists $cave{$n};
        }
    }
    return @ns;
}
