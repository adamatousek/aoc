#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

my %rootdir;
my $cwd = \%rootdir;

my $du = 0;

while ( <> ) {
    if ( /^\$ cd (.*)/ ) {
        $cwd = $cwd->{$1} //= { '..' => $cwd };
    }
    elsif ( /^(\d+) (.*)/ ) {
        $cwd->{$2} = $1;
        $du += $1;
    }
}

my $df = 70_000_000 - $du;
my $df_need = 30_000_000;

my $res_a = 0;
my $res_b = $du;

sub solve($dir) {
    return $dir unless ref $dir;
    my $dirsz = 0;
    for ( grep !/^\.\.$/, keys %$dir ) {
        $dirsz += solve( $dir->{$_} );
    }
    $res_a += $dirsz if $dirsz <= 100_000;
    if ( $df + $dirsz >= $df_need && $dirsz < $res_b ) {
        $res_b = $dirsz;
    }
    return $dirsz;
}

solve(\%rootdir);
say $res_a;
say $res_b;
