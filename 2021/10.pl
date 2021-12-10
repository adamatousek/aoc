#!/usr/bin/perl

use strict;
use warnings;

my %ps = (
    ')' => { o => '(', score => 3},
    ']' => { o => '[', score => 57},
    '}' => { o => '{', score => 1197},
    '>' => { o => '<', score => 25137},
);

my %rps = (
    '(' => 1,
    '[' => 2,
    '{' => 3,
    '<' => 4,
);

my $sum_a = 0;
my @bs;

LINE:
while ( <> ) {
    my @cs = split //;
    chomp

    my @s;
    foreach ( @cs ) {
        push @s, $_ if /[[({<]/;
        if ( /[])}>]/ ) {
            if ( @s && $ps{$_}{o} eq $s[-1] ) {
                pop @s;
            } else {
                print "$s[-1] but $_\n";
                $sum_a += $ps{$_}->{score};
                next LINE;
            }
        }
    }

    my $x = 0;
    while ( @s ) {
        $x = $x * 5 + $rps{ pop @s };
    }
    push @bs, $x if $x > 0;
}

@bs = sort { $a <=> $b } @bs;

print "$sum_a\n";
print $bs[ $#bs / 2 ];
