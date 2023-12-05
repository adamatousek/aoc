#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

$_ = <>;
chomp;
for my $l ( 4, 14 ) {
    my $re = build_re( $l );
    /$re/;
    say "$+[0]";
}

sub build_re($n) {
    my $re = '(.)';
    for my $m (1 .. $n - 1) {
        my $alts = join '|', map "\\g$_", 1 .. $m;
        $re .= "((?!$alts).)"
    }
    return $re;
}
