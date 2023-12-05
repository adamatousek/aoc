#!/usr/bin/perl

use v5.34;
use strict;
use warnings;
use utf8;
use experimental 'signatures';
use Data::Dumper;
use List::Util qw/any min/;
use Clone 'clone';

sub atoi { ord($_[0]) - ord('A') }
sub itoa { chr($_[0] + ord('A')) }

my @deps;
my @deps2;
while ( <> ) {
    my ($dep, $target) = map atoi($_), /tep (.)/g;
    $deps[$target][$dep] = 1;
    $deps2[$target][$dep] = 1;
}
my ($DURATION_BASE, $WORKERS) = @deps > 10 ? (60, 5) : (0, 2);

say [ run(1, @{clone(\@deps)}) ]->[0];
say [ run($WORKERS, @deps2) ]->[1];


sub duration { 1 + $_[0] + $DURATION_BASE }
sub run($workers, @deps) {
    my @ws;
    push @ws, { until => 0 } for 1 .. $workers;
    my ($str, $last_clock);

    my %visited;
    STEP:
    while (1) {
        my $clock = min map $_->{until}, @ws;
        return $str, $last_clock if $clock == 'inf';
        $last_clock = $clock;

        for my $w (@ws) {
            next if $w->{until} > $clock;;
            $w->{until} = 'inf';
            my $t = $w->{task};
            next unless defined $t;

            # task is done
            $w->{task} = undef;
            $deps[$_][$t] = 0 for 0 .. $#deps;
            $str .= itoa($t);
        }

        ENQUEUE:
        for my $w (@ws) {
            next if defined $w->{task};
            for my $s ( 0 .. $#deps ) {
                next if $visited{$s};
                next if any {$_ // 0} $deps[$s]->@*;
                $w->{task} = $s;
                $w->{until} = $clock + duration( $s );
                $visited{$s} = 1;
                next ENQUEUE;
            }
            last;
        }
    }
}
