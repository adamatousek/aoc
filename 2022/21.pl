#!/usr/bin/env perl
use strict;
use warnings;
use v5.36;
use utf8;

use Data::Dumper;
use List::Util qw/max/;

my %ms;
while ( <> ) {
    /^(\w+):/ or next;
    my $m = { id => $1 };
    $ms{$1} = $m;
    $m->{val} = 0+$1 if /(\d+)/;
    if ( /(\w+) ([-\+\*\/]) (\w+)/ ) {
        $m->{l} = $1;
        $m->{op} = $2;
        $m->{r} = $3;
    }
}

say [ solve( 'root' ) ]->[0];

$ms{root}{op} = '-';
say solve_human( 'root', 0 );

sub solve($id) {
    my $m = $ms{$id};
    my $taint = $id eq 'humn';
    return $m->{val}, $m->{taint} //= $taint if defined $m->{val};
    my ($l, $l_taint) = solve($m->{l});
    my ($r, $r_taint) = solve($m->{r});
    my $v = $m->{op} eq '+' ? $l + $r
          : $m->{op} eq '-' ? $l - $r
          : $m->{op} eq '*' ? $l * $r
          : $m->{op} eq '/' ? $l / $r
          : die 'unknown op';
    $m->{val} = $v;
    $taint ||= $l_taint || $r_taint;
    $m->{taint} = $taint;
    return $v, $taint;
}

sub solve_human($id, $val) {
    return $val if $id eq 'humn';

    my $m = $ms{$id};
    my $l = $ms{$m->{l}};
    my $r = $ms{$m->{r}};

    die if $l->{taint} == $r->{taint};

    if ( $l->{taint} ) {
        my $rv = $r->{val};
        my $v = $m->{op} eq '+' ? $val - $rv
              : $m->{op} eq '-' ? $val + $rv
              : $m->{op} eq '*' ? $val / $rv
              : $m->{op} eq '/' ? $val * $rv
              : die 'unknown op';
        return solve_human( $l->{id}, $v );
    }
    elsif ( $r->{taint} ) {
        my $lv = $l->{val};
        my $v = $m->{op} eq '+' ? $val - $lv
              : $m->{op} eq '-' ? $lv - $val
              : $m->{op} eq '*' ? $val / $lv
              : $m->{op} eq '/' ? $val / $lv
              : die 'unknown op';
        return solve_human( $r->{id}, $v );
    }
    else {
        print Dumper $m;
        die;
    }
}
