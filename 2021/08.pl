#!/usr/bin/perl

use strict;
use warnings;

my $count_a = 0;
my $sum_b = 0;

my %lens = (
    2 => [1],
    3 => [7],
    4 => [4],
    5 => [2, 3, 5],
    6 => [0, 6, 9],
    7 => [8],
);

while (<>) {
    my ($ins, $outs) = split /\|/, $_;
    my @ins  = $ins =~ /([a-g]+)/g;
    my @outs = $outs =~ /([a-g]+)/g;
    $count_a += grep { $lens{length $_}->@* == 1  } @outs;

    my %dig;
    ($dig{1}) = map to_mask($_), grep { length == 2 } @ins;
    ($dig{7}) = map to_mask($_), grep { length == 3 } @ins;
    ($dig{4}) = map to_mask($_), grep { length == 4 } @ins;
    ($dig{8}) = map to_mask($_), grep { length == 7 } @ins;
    my $a = $dig{1} ^ $dig{7};
    my $abcdf = $dig{7} | $dig{4};
    ($dig{9}) = grep { ($_ & $abcdf) == $abcdf } map to_mask($_), grep { length == 6 } @ins;
    my $g = $dig{9} ^ $abcdf;
    my $e = $dig{8} ^ $dig{9};
    my $bdg = $dig{7} ^ $dig{9};
    ($dig{5}) = grep { ($_ & $bdg) == $bdg } map to_mask($_), grep { length == 5 } @ins;
    $dig{6} = $dig{5} | $e;
    my $c = $dig{8} ^ $dig{6};
    my $f = $dig{1} ^ $c;
    my $aceg = $a | $c | $e | $g;
    ($dig{2}) = grep { ($_ & $aceg) == $aceg } map to_mask($_), grep { length == 5 } @ins;
    my $d = $dig{2} ^ $aceg;
    $dig{0} = $dig{8} ^ $d;
    $dig{3} = $a | $c | $d | $f | $g;

    my %rdig = reverse %dig;

    $sum_b += join '', map { $rdig{to_mask($_)} } @outs;
}

print "$count_a\n$sum_b\n";

sub to_mask {
    my $rep = shift;
    my $m = 0;
    for my $i (0 .. 6) {
        my $c = chr( $i + ord 'a' );
        $m += 1 << $i if $rep =~ /$c/;
    }
    return $m;
}
