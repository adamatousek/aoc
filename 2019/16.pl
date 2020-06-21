#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ min max sum ];

my @base_pattern = (0,1,0,-1);
my @file_in = <> =~ /(\d)/g;
my @in;
push @in, @file_in for (1..10000);

my $start = 5975677;

say @in[$start..$start+7];
say "Starting";
my @a = @in[$start .. @in-1];
foreach my $ph (1..100) {
    for (my $i = @a-1; $i > 0; --$i) {
        $a[$i-1] += $a[$i];
        $a[$i-1] %= 10;
    }
    say "Phase $ph done";
}

say @a[0..7];

sub do_phase {
    for (my $i = $start; $i < @in ; $i++) {
        @in[$i] = nth_digit( $i );
    }
}

sub nth_digit {
    my $m = shift;
    my $line_sum = 0;
    for (my $i = $m; $i < @in; $i++) {
        $line_sum += $in[$i] * nth_pattern_digit( $i, $m );
    }
    return (abs($line_sum) % 10);
}

sub nth_pattern_digit {
    use integer;
    my $i = shift;
    my $m = shift;

    ++$m;
    ++$i;

    my $period = 4 * $m;
    $i %= $period;
    $i /= $m;
    return $base_pattern[$i];
}
