#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ max ];
use Math::Utils qw[ gcd ];

my @a = ();
while ( <> ) {
    chomp;
    my @l = map { $_ eq '#' ? 1 : 0 } ((split "", $_));
    push (@a, \@l );
}

my $w = +@{$a[0]};
my $h = +@a;

my $m = 0;
my @mc = (0, 0);
my @maukl = ();
my @aukl = ();

for (my $y = 0; $y < $h; $y++) {
    for (my $x = 0; $x < $w; $x++) {
        next if $a[$y]->[$x] == 0;
        @aukl = ();
        my $los = count_los($x, $y);
        if ( $los > $m ) {
            $m = $los;
            @mc = ($x, $y);
            @maukl = @aukl;
        }
    }
}

say $m;
say Dumper @mc;

my @sorted = sort { $a->[0] <=> $b->[0] } @maukl;
say Dumper $sorted[198]; # The asteroid above gets tangens of pi, not -pi.

# 8 16

sub count_los {
    my ($ax, $ay) = @_;
    my $seen = 0;
    for (my $y = 0; $y < $h; $y++) {
        for (my $x = 0; $x < $w; $x++) {
            next if $a[$y]->[$x] == 0;
            next if $ax == $x && $ay == $y;

            my @v = ($x - $ax, $y - $ay);
            my $d = abs( my_gcd( @v ) );
            my $do_count = 1;
            if ($d > 1) {
                my @lcv = map { $_ / $d } @v;
                for (my $i = 1; $i < $d; $i++) {
                    my @v2 = map { $_ * $i } @lcv;
                    if ( $a[$ay + $v2[1]]->[$ax + $v2[0]] == 1 ) {
                        $do_count = 0;
                        last;
                    }
                }
            }
            if ( $do_count ) {
                $seen++;
                my $ang = atan2(-$v[0], $v[1]);
                push @aukl, ([ $ang, $x, $y]);
            }
        }
    }
    return $seen;
}

sub my_gcd {
    my( $x, $y ) = @_;
    return $x if $y == 0;
    return $y if $x == 0;
    return gcd( $x, $y );
}
