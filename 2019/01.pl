#$st->{name} =~ s/,? /_/g;!/usr/bin/perl
use v5.16;
use strict;
use warnings;

my $s = 0;
while (<>) {
    my $f = fuel( $_ );
    while ( $f > 0 ) {
        $s += $f;
        $f = fuel( $f );
    }
}
print $s;

sub fuel
{
    my ($w) = @_;
    return int( $w / 3 ) - 2;
}
