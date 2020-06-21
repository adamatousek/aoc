#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ shuffle min max any sum ];

my $DBG = 0;

my $INF = 9999999;

# "x,y" => { coord => "x,y", s => [\...], b => 0 }
my %maze;
my @maze;

my @startpos;

my $x = 0;
my $y = 0;
my $w = 1;
while (my $line = <>) {
    chomp $line;
    foreach my $c ($line =~ /./g) {
        next if $x == 2 && $y == 2;
        my $coord = "0:$x,$y";
        my $tile = { coord => "$coord", s => [], b => 0+($c eq '#'), w => $w };
        $maze{$coord} = $tile;
        push @maze, $tile;
        my $towest = "0:" . ($x - 1) . ",$y";
        my $tonorth = "0:$x," . ($y - 1);
        mutconnect($coord, $towest);
        mutconnect($coord, $tonorth);
        $w <<= 1;
    } continue {
        ++$x;
    }
    $x = 0;
    ++$y;
}

my $mind = 0;
my $maxd = 0;
my %seen;
my $rating;

for (my $i = 0; $i < 200; ++$i ) {
    #$rating = 0;
    #foreach my $t (@maze) {
    #    $rating += $t->{w} if $t->{b};
    #}
    #last if defined $seen{$rating};
    #$seen{$rating} = 1;

    add_inner_layer() if inner_needed();
    add_outer_layer() if outer_needed();

    foreach my $t (@maze) {
        my $adj = sum map { $_->{b} } @{$t->{s}};
        if ( $t->{b} && $adj != 1 ) {
            $t->{nextb} = 0;
        } elsif ( ! $t->{b} && ($adj == 1 || $adj == 2) ) {
            $t->{nextb} = 1;
        } else {
            $t->{nextb} = $t->{b};
        }
    }
    foreach my $t (@maze) {
        $t->{b} = $t->{nextb};
    }
}

#say "Repeated rating: $rating";
say "Sum: ", sum map { $_->{b} } @maze;

exit;

sub create_layer {
    my $d = shift;
    foreach my $y (0..4) {
        foreach my $x (0..4) {
            my $coord = "$d:$x,$y";
            next if $x == 2 && $y == 2;
            my $tile = { coord => "$coord", s => [], b => 0, w => 0 };
            $maze{$coord} = $tile;
            push @maze, $tile;
            my $towest = "$d:" . ($x - 1) . ",$y";
            my $tonorth = "$d:$x," . ($y - 1);
            mutconnect($coord, $towest);
            mutconnect($coord, $tonorth);
        }
    }
}

sub ringconnect {
    my ($inner, $outer) = @_;
    foreach my $i (0..4) {
        mutconnect( "$outer:2,1", "$inner:$i,0" );
        mutconnect( "$outer:2,3", "$inner:$i,4" );
        mutconnect( "$outer:1,2", "$inner:0,$i" );
        mutconnect( "$outer:3,2", "$inner:4,$i" );
    }
}

sub add_inner_layer {
    say "Adding inner layer ", $maxd + 1;
    create_layer( $maxd + 1 );
    ringconnect( $maxd + 1, $maxd );
    ++$maxd;
}

sub add_outer_layer {
    say "Adding outer layer ", $mind - 1;
    create_layer( $mind - 1 );
    ringconnect( $mind, $mind - 1 );
    --$mind;
}

sub bugged {
    my $coord = shift;
    return $maze{$coord}->{b};
}

sub inner_needed {
    return any {bugged "$maxd:$_"} ("1,2", "2,1", "3,2" , "2,3");
}

sub outer_needed {
    return 1 if any {bugged "$mind:0,$_"} (0..4);
    return 1 if any {bugged "$mind:4,$_"} (0..4);
    return 1 if any {bugged "$mind:$_,0"} (0..4);
    return 1 if any {bugged "$mind:$_,4"} (0..4);
    return 0;
}

#my $FULL_KEYS = hash_keys(grep {iskey $_} keys %clefs);
#say "Full key flags: $FULL_KEYS";
#
#my $min_steps = $INF;
#my @stack = ({ have => 0, positions => ["_0","_1","_2","_3"], steps => 0 });
#
#while (my $st = pop @stack) {
#    my $steps = $st->{steps};
#    next if ( $steps >= $min_steps );
#    my $have = $st->{have};
#    my @positions = @{$st->{positions}};
#    my $st_desc = "@positions:$have";
#    next if ($memo{$st_desc} // $INF) <= $steps;
#    $memo{$st_desc} = $steps;
#    print "--------------------------------------------\nPopped: " if $DBG;
#    print Dumper $st if $DBG;
#    say "Acquired keys: $have = ", (unhash_keys( $have )) if $DBG;
#    if ( $have == $FULL_KEYS ) {
#        say "new min: $steps" if $steps < $min_steps;
#        $min_steps = min $min_steps, $steps;
#        next;
#    }
#
#    my @next_targets = unhash_keys( list_reachable( $have ) & ~$have );
#    say "Reachable keys: ", list_reachable( $have ), " = ", unhash_keys( list_reachable( $have ) ) if $DBG;
#    say "Interested in keys: ", @next_targets if $DBG;
#
#    foreach my $k (@next_targets) {
#        my $new_have = $have | (hash_keys($k));
#        my $who_moves = $clefs{ $k }->{bot};
#        my $new_steps = $steps + $clefs{ $positions[$who_moves] }->{dist}->{$k};
#        my $new_positions = [@positions];
#        $new_positions->[ $who_moves ] = $k;
#        push @stack, { have => $new_have, positions => $new_positions, steps => $new_steps };
#    }
#}
#
#say "Min steps: $min_steps";


sub mutconnect {
    my ($u, $v) = @_;
    my $ru = $maze{$u};
    my $rv = $maze{$v};
    return unless defined $ru and defined $rv;
    push @{$ru->{s}}, $rv;
    push @{$rv->{s}}, $ru;
}

