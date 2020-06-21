#!/usr/bin/perl
use v5.16;
use strict;
use warnings;

my @mem;
my @orig_mem;
while (<>) {
    push( @orig_mem, $_ );
}

my $noun = 0;
my $verb = 0;
my $sum = 0;
my $spl = 0;
do {
    @mem = @orig_mem;
    $noun = $spl;
    $verb = $sum - $spl;
    $mem[1] = $noun;
    $mem[2] = $verb;

    my $ip = 0;
    while ( $mem[$ip] != 99 ) {
        my @inst = @mem[$ip .. $ip+3];
        my $lhs = $mem[$inst[1]];
        my $rhs = $mem[$inst[2]];
        my $res;
        if ( $inst[0] == 1 ) {
            $res = $lhs + $rhs;
        } elsif ( $inst[0] == 2 ) {
            $res = $lhs * $rhs;
        } else {
            die "Unknown opcode " . $inst[0];
        }
        $mem[$inst[3]] = $res;
        $ip += 4;
    }

    $spl++;
    if ( $spl > $sum ) {
        $sum++;
        $spl = 0;
    }
} while ( $mem[0] != 19690720 );

print "noun: " . $noun . ", verb: " . $verb . ", result = " . $mem[0];
