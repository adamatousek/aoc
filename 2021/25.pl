#!/usr/bin/perl

use warnings;
use strict;

my @cs = map [/[>v.]/g], <>;
#print @$_, "\n" foreach @cs;

my $steps = 1;
$steps++ while step();
print "$steps\n";

sub step {
    my $moved = 0;
    for my $row ( @cs ) {
        for my $x ( 0 .. $#$row ) {
            my $next = ( $x + 1 ) % @$row;
            if ( $row->[ $x ] eq '>' && $row->[ $next ] eq '.' ) {
                $row->[ $x ] = $x ? '.' : '_';
                $row->[$next] = $next ? '-' : '>';
                $moved = 1;
            }
            elsif ( $row->[ $x ] eq '-' ) {
                $row->[ $x ] = '>';
            }
        }
        $row->[ 0 ] = '.' if $row->[ 0 ] eq '_';
    }

    for my $x ( 0 .. $cs[0]->$#* ) {
        while ( my ($y, $row) = each @cs ) {
            my $next = ( $y + 1 ) % @cs;
            if ( $row->[ $x ] eq 'v' && $cs[ $next ][ $x ] eq '.' ) {
                $row->[ $x ] = $y ? '.' : ':';
                $cs[ $next ][ $x ] = $next ? '|' : 'v';
                $moved = 1;
            }
            elsif ( $row->[ $x ] eq '|' ) {
                $row->[ $x ] = 'v';
            }
        }
        $cs[ 0 ][ $x ] = '.' if $cs[ 0 ][ $x ] eq ':';
    }
    return $moved;
}

