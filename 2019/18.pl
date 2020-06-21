#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ shuffle min max any sum ];

my $DBG = 0;

sub iskey {
    return (shift @_) =~ /[a-z]/;
}
sub isdoor {
    return (shift @_) =~ /[A-Z]/;
}
sub keyof {
    return chr( ord(shift @_) + ord('a') - ord('A') );
}

my $INF = 9999999;

# "x,y" => { coord => "x,y", s => [\...], d => "a" }
my %maze;
my %clefs;
my @quadrants;

my @startpos;

my $x = 0;
my $y = 0;
my $q = 0;
while (my $line = <>) {
    foreach my $c ($line =~ /./g) {
        my $coord = "$x,$y";
        next if $c eq '#';
        $maze{$coord} = { coord => "$coord", s => [], d => $c };
        push @startpos, $coord if $c eq '@';
        $clefs{"_@{[$q++]}"} = { coord => $coord, h => 0 } if $c eq '@';
        $clefs{$c} = { coord => $coord, h => hash_keys($c) } if iskey $c;
        my $towest = ($x - 1) . ",$y";
        my $tonorth = "$x," . ($y - 1);
        mutconnect($coord, $towest);
        mutconnect($coord, $tonorth);
    } continue {
        ++$x;
    }
    $x = 0;
    ++$y;
}


my %seen;

say "Keys: ", 0+(keys %clefs);

say "Robots ", 0+@startpos;

my $i = 0;
check_tree($_, $i++) for @startpos;
calc_dist($_) for keys %clefs;

print Dumper \%clefs;

my %memo;

# exit;

my $FULL_KEYS = hash_keys(grep {iskey $_} keys %clefs);
say "Full key flags: $FULL_KEYS";

my $min_steps = $INF;
my @stack = ({ have => 0, positions => ["_0","_1","_2","_3"], steps => 0 });

while (my $st = pop @stack) {
    my $steps = $st->{steps};
    next if ( $steps >= $min_steps );
    my $have = $st->{have};
    my @positions = @{$st->{positions}};
    my $st_desc = "@positions:$have";
    next if ($memo{$st_desc} // $INF) <= $steps;
    $memo{$st_desc} = $steps;
    print "--------------------------------------------\nPopped: " if $DBG;
    print Dumper $st if $DBG;
    say "Acquired keys: $have = ", (unhash_keys( $have )) if $DBG;
    if ( $have == $FULL_KEYS ) {
        say "new min: $steps" if $steps < $min_steps;
        $min_steps = min $min_steps, $steps;
        next;
    }

    my @next_targets = unhash_keys( list_reachable( $have ) & ~$have );
    say "Reachable keys: ", list_reachable( $have ), " = ", unhash_keys( list_reachable( $have ) ) if $DBG;
    say "Interested in keys: ", @next_targets if $DBG;

    foreach my $k (@next_targets) {
        my $new_have = $have | (hash_keys($k));
        my $who_moves = $clefs{ $k }->{bot};
        my $new_steps = $steps + $clefs{ $positions[$who_moves] }->{dist}->{$k};
        my $new_positions = [@positions];
        $new_positions->[ $who_moves ] = $k;
        push @stack, { have => $new_have, positions => $new_positions, steps => $new_steps };
    }
}

say "Min steps: $min_steps";


sub mutconnect {
    my ($u, $v) = @_;
    my $ru = $maze{$u};
    my $rv = $maze{$v};
    return unless defined $ru and defined $rv;
    push @{$ru->{s}}, $rv;
    push @{$rv->{s}}, $ru;
}

sub reachable_keys {
    my ($have, $from) = @_;
    my $hk = hash_keys(keys %$have);
    my $h = "$from->{coord}:$hk";
    my $st = $seen{"$h"};
    return $st if defined $st;
    my $s = { rkeys => do_reachable_keys($have, $from), steps => $INF };
    say "New state $h" if $DBG;
    say Dumper $s->{rkeys} if $DBG;
    $seen{"$h"} = $s;
    return $s;
}

sub do_reachable_keys {
    my ($have, $from) = @_;
    say "BFS from $from->{coord}, having ", %$have if $DBG;
    my $found = {};
    my $vis = { "$from->{coord}" => 0 };
    my @q = ($from);
    while (my $v = shift @q) {
        my $c = $v->{d};
        my $w = $vis->{"$v->{coord}"} ;
        $found->{"$c"} = $w if (iskey $c and not $have->{"$c"});
        if ( isdoor $c ) {
            my $kod = keyof $c;
            next unless defined $have->{"$kod"};
        }
        my $nextw = $vis->{"$v->{coord}"} + 1;
        foreach my $s (@{$v->{s}}) {
            next if defined $vis->{"$s->{coord}"};
            push @q, $s;
            $vis->{"$s->{coord}"} = $nextw;
        }
    }
    return $found;
}

sub hash_keys {
    my $s = 0;
    foreach my $k (@_) {
        $k = keyof($k) if isdoor($k);
        $s += 1 << (ord("$k") - ord('a'));
    }
    return $s;
}

sub unhash_keys {
    my $kh = shift;
    my @ks;
    my $k = ord 'a';
    while ($kh > 0) {
        push @ks, chr($k) if ($kh & 1);
        $kh >>= 1;
        ++$k;
    }
    return (@ks == 1) ? $ks[0] : @ks;
}

sub check_tree {
    my $coord = shift;
    my $robot_idx = shift;
    print "From $coord: " if $DBG;
    my $vis = {};
    dfs( $coord, $coord, $vis, 0, $robot_idx );
    say " ok."
}

sub dfs {
    my ( $coord, $parent, $vis, $doors, $bot ) = @_;
    my $v = $maze{$coord};
    my $c = $v->{d};
    $vis->{$coord} = 1;
    if (iskey $c) {
        say "    key $c depends on ", unhash_keys($doors) if $DBG;
        $clefs{$c}->{dep} = $doors;
        $clefs{$c}->{bot} = $bot;
        push @{$quadrants[$bot]}, $c;
    }
    my $doorflag = isdoor($c) ? hash_keys($c) : 0;
    foreach my $s (@{$v->{s}}) {
        my $sc = $s->{coord};
        next if $sc eq $parent;
        die "cycle ($vis->{$sc}) found at $sc; $parent" if $vis->{$sc};
        dfs( $sc, $coord, $vis, $doors | $doorflag, $bot );
    }
    $vis->{$coord} = 2;
}

sub list_reachable {
    my $have = shift;
    return sum (map {(iskey($_) && ($clefs{$_}->{dep} & ~$have)) ? 0 : $clefs{$_}->{h} } (keys %clefs));
}

sub calc_dist {
    my ($from, $quadrant) = @_;
    my $vis = {};
    my $coord = $clefs{$from}->{coord};
    say "Calculating distances from key $from at $coord" if $DBG;
    dist_dfs( $coord, $coord, $vis, $quadrant, 0, $from );
}
sub dist_dfs {
    my ( $coord, $parent, $vis, $bot, $w, $from ) = @_;
    my $v = $maze{$coord};
    my $c = $v->{d};
    $vis->{$coord} = 1;
    if (iskey $c) {
        say "    key $c in $w steps" if $DBG;
        $clefs{$from}->{dist}->{$c} = $w;
    }
    foreach my $s (@{$v->{s}}) {
        my $sc = $s->{coord};
        next if $sc eq $parent;
        die "cycle ($vis->{$sc}) found at $sc; $parent" if $vis->{$sc};
        dist_dfs( $sc, $coord, $vis, $bot, $w + 1, $from );
    }
    $vis->{$coord} = 2;
}
