#!/usr/bin/perl

use warnings;
use strict;

use List::Util qw/sum/;

my @schools = (0) x 9;

foreach ( <> =~ /(\d+)/g ) {
    $schools[$_]++;
}

print "@schools\n";

for (1..256) {
    step();
    print "$_: ", sum(@schools), "\n"
        if $_ ~~ [18, 80, 256];
};


sub step {
    my $ready = shift @schools;
    push @schools, $ready;
    $schools[6] += $ready;
}
