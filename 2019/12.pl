#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ all ];
use Math::Utils qw[ lcm ];

# t1:
# <x=2, y=-10, z=-7>
# <x=4, y=-8, z=8>
# <x=3, y=5, z=-1>
my @test_moons = (
    { id => 0, pos => [-1, 0, 2], vel => [0, 0, 0] },
    { id => 1, pos => [2, -10, -7], vel => [0, 0, 0] },
    { id => 2, pos => [4, -8, 8], vel => [0, 0, 0] },
    { id => 3, pos => [3, 5, -1], vel => [0, 0, 0] }
);
my @my_moons = (
    { id => 0, pos => [4, 12, 13], vel => [0, 0, 0] },
    { id => 1, pos => [-9, 14, -3], vel => [0, 0, 0] },
    { id => 2, pos => [-7, -1, 2], vel => [0, 0, 0] },
    { id => 3, pos => [-11, 17, -1], vel => [0, 0, 0] }
);
#<x=4, y=12, z=13>
#<x=-9, y=14, z=-3>
#<x=-7, y=-1, z=2>
#<x=-11, y=17, z=-1>

my $moons = \@my_moons;

my @states = ( {}, {}, {} );
my @loop_len = ( 0, 0, 0 );
my @loop_start = ( 0, 0, 0 );

my $count = 0;
my $b;
do {
    foreach my $i (0..2) {
        next if ( $loop_len[$i] );
        my $state_desc = "";
        foreach my $m ( @$moons ) {
            $state_desc .= $m->{pos}->[$i] . ':' .  $m->{vel}->[$i] . '|';
        }
        if (not defined $states[$i]->{$state_desc}) {
            $states[$i]->{$state_desc} = $count;
        } else {
            $loop_start[$i] = $states[$i]->{$state_desc};
            $loop_len[$i] = $count - $loop_start[$i];
        }
    }
    step_moons( $moons );
    ++$count;
} until ( all ( sub { $_ > 0 }, @loop_len ) );

say "Steps: ", $count;
say "Loop starts";
print Dumper @loop_start;
say "Loop lengths";
print Dumper @loop_len;

say "Universe period: ", lcm @loop_len;

sub step_moons {
    my $moons = shift;
    foreach my $m1 ( @$moons ) {
        foreach my $m2 ( @$moons ) {
            next if $m1->{id} == $m2->{id};
            for (my $coord = 0; $coord < 3; $coord++) {
                $m1->{vel}->[$coord] -= $m1->{pos}->[$coord] <=> $m2->{pos}->[$coord];
            }
        }
    }
    foreach my $m ( @$moons ) {
        for (my $coord = 0; $coord < 3; $coord++) {
            $m->{pos}->[$coord] += $m->{vel}->[$coord];
        }
    }
}

#my $e_p = 0;
#my $e_k = 0;
#my $e = 0;
#foreach my $m ( @moons ) {
#    $e_k = sum( map abs, @{$m->{vel}} );
#    $e_p = sum( map abs, @{$m->{pos}} );
#    $e += $e_p * $e_k;
#}
#say $e;
