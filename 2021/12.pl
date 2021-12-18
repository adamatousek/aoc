#!/usr/bin/perl

use warnings;
use strict;

my %g;
while ( <> ) {
    /(\w+)-(\w+)/ or next;
    push $g{$1}->@*, $2 unless $2 eq 'start';
    push $g{$2}->@*, $1 unless $1 eq 'start';
}

print count_paths( 'start' ), "\n";
print count_paths_b( 'start', '' ), "\n";

sub count_paths {
    my ($v, @seen) = @_;
    return 1 if $v eq 'end';
    push @seen, $v if $v =~ /[a-z]/;
    my $count = 0;
    for my $s ( $g{$v}->@* ) {
        next if grep { $_ eq $s } @seen;
        $count += count_paths( $s, @seen );
    }
    return $count;
}

sub count_paths_b {
    my ($v, $twice, @seen) = @_;
    return ( $twice !~ /[a-z]/ ) if $v eq 'end';
    return 0 if grep { $_ eq $v } ( @seen );
    my $count = 0;
    for my $s ( $g{$v}->@* ) {
        if ( $v =~ /[a-z]/ ) {
            if ( !$twice ) {
                $count += count_paths_b( $s, '', $v, @seen );
                $count += count_paths_b( $s, $v, @seen );
            } elsif ( $twice eq $v ) {
                $count += count_paths_b( $s, '_', $v, @seen );
            } else {
                $count += count_paths_b( $s, $twice, $v, @seen );
            }
        } else {
            $count += count_paths_b( $s, $twice, @seen );
        }
    }
    return $count;
}
