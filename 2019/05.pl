#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';

our @inputs = (5);

our @mem;
our @inst;
our @mode;
our $ip;

sub load {
    my @indices = @_;
    my @vals = map { ($mode[$_] == 1) ? $inst[$_+1] : $mem[$inst[$_+1]] } @indices;
    return @vals == 1 ? $vals[0] : @vals;
}

sub input {
    state $inputidx = 0;
    return $inputs[$inputidx++];
}

my @orig_mem = <> =~ /(-?\d+)/g;

@mem = @orig_mem;

$ip = 0;
while ( $mem[$ip] != 99 ) {
    my $opcode = $mem[$ip];
    my $arity = op_arity( $opcode );
    @mode = op_mode( $opcode );
    $opcode = $opcode % 100;
    @inst = @mem[$ip .. $ip+$arity];

    if ( $arity == 3 ) {
        my ($lhs, $rhs) = load 0, 1;
        my $res;
        given ( $opcode ) {
            when ( 1 ) { $res = $lhs + $rhs; }
            when ( 2 ) { $res = $lhs * $rhs; }
            when ( 7 ) { $res = $lhs < $rhs; }
            when ( 8 ) { $res = $lhs == $rhs; }
            default { die "Unknown opcode " . @inst; }
        }
        $mem[$inst[3]] = $res;
    } elsif ( $arity == 2 ) {
        my ($test, $dest) = load 0, 1;
        if ( ($opcode == 5) xor ($test == 0) ) {
            $ip = $dest;
            next;
        }
    } elsif ( $arity == 1 ) {
        given ( $opcode ) {
            when (3) { $mem[$inst[1]] = input(); }
            when (4) { say "OUTPUT: ", load 0; }
        }
    } else {
        die "wtf, man";
    }
    $ip += $arity + 1;
}

say "DONE.";

sub op_arity {
    my ($op) = @_;
    $op = $op % 100;
    return 3 if ( $op ~~ [1,2,7,8] );
    return 2 if ( $op ~~ [5,6] );
    return 1 if ( $op ~~ [3,4] );
    die "FATAL: unknown opcode ", $op;
}

sub op_mode {
    my ($o) = @_;
    my $ar = op_arity( $o );
    $o = int( $o / 10 );
    my @mode = ();
    while ( $ar > 0 ) {
        $o = int( $o / 10 );
        push( @mode, $o % 10 );
        $ar--;
    }
    return @mode;
}

