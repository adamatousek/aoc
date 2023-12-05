#!/usr/bin/perl

use warnings;
use strict;
use Data::Dumper;
use List::Util qw/sum all max/;
use feature 'say';

my $MIN_OVERLAP = 12;

my @scanners;
my %lengths;
my $scanner;
my %shared;

while (<>) {
    chomp;
    if (/scanner (\d+)/) {
        $scanner = { id => scalar @scanners, inputs => [] };
        push @scanners, $scanner;
    }
    elsif (/\d/) {
        push $scanner->{inputs}->@*, [ split ',' ]
    }
}

for my $s (@scanners) {
     my @lines = listprodmap( \&mkline, $s->{inputs}->@* );
     for my $l (@lines) {
         for my $peer_id ( $lengths{ $l->{len} }->@* ) {
             die "Non-unique distance" if $peer_id == $s->{id};
             my ($s1, $s2) = sort { $a <=> $b } ($peer_id, $s->{id});
             push $shared{"$s1;$s2"}->@*, $l->{len};
         }
         push $lengths{ $l->{len} }->@*, $s->{id};
     }
     $s->{lines} = \@lines;
}

my @candidate_pairs = map [split ';'],
                      grep { $shared{$_}->@* >= ( $MIN_OVERLAP * ($MIN_OVERLAP - 1) ) / 2 }
                      keys %shared
;

# Now I know that distances perceived by each sensor are unique, but that
# doesn't mean than a line of the same length perceived by two sensors must
# connect the same beacons. I assume, however, that when beacons between two
# sensors agree in at least $MIN_OVERLAP - 1 edges, they are the same beacon.

print Dumper \@candidate_pairs;

my %map = map {; "@$_" => 1 } $scanners[0]{inputs}->@*;
$scanners[0]->{done} = 1;
$scanners[0]->{origin} = { v => [0, 0, 0], roll => 0, face => 0 };

while ( @candidate_pairs ) {
    my ($old, $new) = pop_candidate( \@candidate_pairs, \@scanners );
    my $ns = $scanners[$new];

    print "Adding $new to $old\n";
    die unless $scanners[$old]{done};
    die if $ns->{done};

    my %m = map_beacons( $old, $new );
    die if %m < $MIN_OVERLAP;
    my $n = find_origin( %m ); # position and rotation of new in old's system
    print Dumper "Scanner $new is at", $n;

    # Transform all beacons to old's coordinate system and add them to the map
    for my $p ( $ns->{inputs}->@* ) {
        # note that we don't rebind the arrayrefs but change their contents, so
        # the transformations propagate to the precomputed lines
        @$p = vec_add( $n->{v}, [ rotate3d( @$p, $n ) ] );
        my $seen = !!$map{"@$p"};
        my $chbs = could_have_been_seen( $p, \@scanners );
        die "Point @$p marked as seen but not in the covered area"
            if $seen && !$chbs;
        die "Point @$p not seen yet but the area has already been covered"
            if !$seen && $chbs;
        $map{"@$p"} = 1;
    }
    $ns->{done} = 1;
    $ns->{origin} = $n;

    # Add new's beacons to the map

    @candidate_pairs = grep { !$scanners[$_->[0]]{done} || !$scanners[$_->[1]]{done} } @candidate_pairs;
}

say "a: ", scalar %map;
say "b: ", max listprodmap( \&manhattan, map( $_->{origin}{v}, @scanners ) );

exit 0;

##############################################################################

sub mkline {
    my ($a, $b) = @_;
    my $dsq = 0;
    for my $i (0 .. $a->$#*) {
        my $d = $a->[$i] - $b->[$i];
        $dsq += $d * $d;
    }
    return { len => $dsq, a => $a, b => $b };
}

sub listprodmap {
    my $f = shift;
    my @res;
    for my $i (1 .. $#_) {
        for my $j (0 .. $i - 1) {
            push @res, $f->( $_[$i], $_[$j] );
        }
    }
    return @res;
}

sub pop_candidate {
    my ($pairs, $scanners) = @_;
    for my $i ( 0 .. $pairs->$#* ) {
        my $p = $pairs->[$i];
        next if !!$scanners[$p->[0]]{done} == !!$scanners[$p->[1]]{done};
        splice @$pairs, $i, 1;
        return $scanners[$p->[0]]{done} ? @$p : $p->@[1,0];
    }
    die "No more candidates to try";
}

sub rotate3d {
    my @v = @_[0..2];
    my $r = $_[3];
    $r = undef unless defined $r->@{qw/roll face/};
    my @rs;
    for my $roll (0 .. 3) {
        if ( !$r || $r->{roll} == $roll ) {
            for my $face (0 .. 5) {
                return @v if $r && $r->{face} == $face;
                push @rs, { roll => $roll, face => $face, v => [ @v ] };
                my $axis = $face % 3;
                @v = $axis == 0 ? (-$v[2],  $v[1],  $v[0])
                   : $axis == 1 ? ( $v[0],  $v[2], -$v[1])
                                : (-$v[1],  $v[0],  $v[2]);
            }
        }
        @v[1,2] = ($v[2], -$v[1]);
    }
    return @rs;
}

sub map_beacons {
    my ($old, $new) = @_;
    my %shared;
    for my $ol ( $scanners[$old]->{lines}->@* ) {
        for my $nl ( $scanners[$new]->{lines}->@* ) {
            next unless $ol->{len} == $nl->{len};
            $shared{"@{$ol->{a}};@{$nl->{a}}"}++;
            $shared{"@{$ol->{a}};@{$nl->{b}}"}++;
            $shared{"@{$ol->{b}};@{$nl->{a}}"}++;
            $shared{"@{$ol->{b}};@{$nl->{b}}"}++;
        }
    }
    my %mapping;
    for my $k ( keys %shared ) {
        next if $shared{$k} < $MIN_OVERLAP - 1;
        my ($op, $np) = split ';', $k;
        die "Ambiguous beacon mapping: $np in sensor $new"
            if defined $mapping{$np};
        $mapping{$np} = $op;
    }
    return %mapping;
}

sub find_origin {
    my %m = @_;
    my %intersect;
    # This looks like it could be a lot simpler, but a naive approach would
    # break on beacons with one of the coordinates zero (i.e., on an axis)
    for my $n ( keys %m ) {
        my @n = map -$_, split ' ', $n;
        my @o = split ' ', $m{$n};
        my %ps;
        for my $p_n ( rotate3d( @n ) ) {
            if ($ps{"@{$p_n->{v}}"}++) {
            }
            my @p_o = vec_add( $p_n->{v}, \@o );
            $p_n->{v} = \@p_o;
            push $intersect{"@p_o"}->@*, $p_n;
        }
    }
    # select the most intersected point
    my ($ps) = sort { @$b <=> @$a } values %intersect;
    die "Origin is agreed on by only @{[scalar @$ps]} beacons"
        if @$ps < $MIN_OVERLAP;
    die "FIXME: Coaxial beacon"
        if @$ps > $MIN_OVERLAP;
    return $ps->[0];
}

sub vec_add {
    my ($a, $b) = @_;
    my @v;
    for my $i ( 0 .. $a->$#* ) {
        push @v, $a->[$i] + $b->[$i];
    }
    return @v;
}

sub could_have_been_seen {
    my ($p, $scanners) = @_;
    for my $s ( @$scanners ) {
        my $o = $s->{origin}->{v};
        next unless $o;
        my @min = vec_add( $o, [-1000, -1000, -1000] );
        my @max = vec_add( $o, [ 1000,  1000,  1000] );
        return 1 if all { $min[$_] <= $p->[$_] <= $max[$_] } (0..2);
    }
    return 0;
}

sub manhattan {
    my ($a, $b) = @_;
    return sum map abs( $a->[$_] - $b->[$_] ), 0 .. 2;
}
