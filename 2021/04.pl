#!/usr/bin/perl

use warnings;
use strict;

use Data::Dumper;

chomp (my $line = <>);
my @numbers = split /,/, $line;
print "@numbers\n";

my @tables;

while ( my $t = read_table() ) {
    push @tables, $t;
}

my ($first, $last);
for my $n (@numbers) {
    for my $t (@tables) {
        next if $t->{done};
        if ( mark($n, $t) ) {
            $last = $n * score( $t );
            $first //= $last;
            $t->{done} = 1;
        }
    }
}
print "$first\n$last\n";


sub read_table {
    my $line = <>;
    return unless defined $line;

    my $t = {};
    for (0 .. 4) {
        chomp( $line = <> );
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;

        my $nums = [split /\s+/, $line];
        push $t->{rows}->@*, $nums;
    }

    for my $i (0 .. 4) {
        push $t->{cols}->@*, [ map { $_->[$i] } ($t->{rows}->@*) ];
    }

    print Dumper $t;
    return $t;
}

sub mark {
    my ($n, $t) = @_;
    my $done = 0;
    for my $dir ("rows", "cols") {
        for my $xs ( $t->{$dir}->@* ) {
            $xs = [ grep { $_ != $n } @$xs ];
            $done = 1 unless @$xs;
        }
    }
    return $done;
}

sub score {
    my $t = shift;
    my $s = 0;
    for my $xs ($t->{rows}->@*) {
        $s += $_ for @$xs;
    }
    return $s;
}

