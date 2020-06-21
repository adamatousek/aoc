#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Algorithm::Permute;
use Data::Dumper;
use List::Util qw[ max ];

my $I_ADD = 1;
my $I_MUL = 2;
my $I_IN = 3;
my $I_OUT = 4;
my $I_JNZ = 5;
my $I_JZ = 6;
my $I_LT = 7;
my $I_EQ = 8;
my $I_HALT = 99;

########################################################################
# INTERPRETER

sub op_arity {
    my ($op) = @_;
    $op = $op % 100;
    return 3 if ( $op ~~ [$I_ADD, $I_MUL, $I_LT, $I_EQ] );
    return 2 if ( $op ~~ [$I_JNZ, $I_JZ] );
    return 1 if ( $op ~~ [$I_IN, $I_OUT] );
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

# Execute until interrupt; returns last opcode
sub run_eval {
    my ( $ctx ) = @_;

    # evaluation state (references)
    my $mem = $ctx->{mem};
    my $ip = $ctx->{ip};
    my $input = $ctx->{input};
    my $output = $ctx->{output};

    # instruction parsing state
    my $opcode;
    my $arity;
    my @mode;
    my @args;

    # helpers
    local *load = sub {
        my @indices = @_;
        #say "Indices";
        #print Dumper @indices;
        my @vals = map { ($mode[$_] == 1) ? $args[$_] : $mem->[$args[$_]] } @indices;
        #say "Vals";
        #print Dumper @vals;
        return @vals == 1 ? $vals[0] : @vals;
    };

    local *advance = sub {
        $ip += $arity + 1;
    };

    while ( ( $opcode = $mem->[$ip] ) != $I_HALT ) {
        $arity = op_arity( $opcode );
        @mode = op_mode( $opcode );
        $opcode = $opcode % 100;
        @args = @$mem[$ip+1 .. $ip+$arity];
        #say "Args";
        #print Dumper @args;

        if ( $arity == 3 ) {
            my ($lhs, $rhs) = load(0, 1);
            my $res;
            given ( $opcode ) {
                when ( $I_ADD ) { $res = $lhs + $rhs; }
                when ( $I_MUL ) { $res = $lhs * $rhs; }
                when ( $I_LT  ) { $res = $lhs < $rhs; }
                when ( $I_EQ  ) { $res = $lhs == $rhs; }
            }
            $mem->[$args[2]] = $res;
        } elsif ( $arity == 2 ) {
            my ($test, $dest) = load(0, 1);
            if ( ($opcode == $I_JNZ) xor ($test == 0) ) {
                $ip = $dest;
                next;
            }
        } elsif ( $arity == 1 ) {
            given ( $opcode ) {
                when ( $I_IN ) {
                    last if @$input == 0;

                    my $in = shift @$input;
                    say "INPUT: ", $in;
                    $mem->[$args[0]] = $in;
                }
                when ( $I_OUT ) {
                    my $out = load(0);
                    push @$output, $out;
                    advance();
                    last;
                }
            }
        } else {
            die "wtf, man";
        }

        advance();
    }

    $ctx->{ip} = $ip;
    return $opcode
}


########################################################################
# MAIN

my @orig_mem;
while ( <> ) {
    push @orig_mem, $_ =~ /(-?\d+)/g;
}

my $max_final = 0;
my $permiter = Algorithm::Permute->new( [5..9] );
while (my @perm = $permiter->next) {

    my $final_out;

    my @contexts;
    my $ctx = 0; # Active task

    foreach my $c (0..4) {
        my @nmem = @orig_mem;
        push @contexts, { mem => \@nmem, ip => 0, input => [ $perm[$c] ], output => [] };
    }
    push @{$contexts[0]->{input}}, 0;


    my $intr_opcode;
    do {
        $intr_opcode = run_eval( $contexts[ $ctx ] );

        if ( $intr_opcode == $I_OUT ) {
            my $out = shift @{$contexts[ $ctx ]->{output}};
            say "OUTPUT: ", $out;
            push @{$contexts[ ($ctx + 1) % 5 ]->{input}}, $out;
            $final_out = $out if $ctx == 4;

            say "CONTEXT SWITCH from ", $ctx;
            $ctx = ($ctx + 1) % 5;

        } elsif ( $intr_opcode == $I_IN ) {
            die "FATAL: Task " . $ctx . " has nothing to read";

        } else {
            say "Interrupt ", $intr_opcode;
        }
    } until ( $intr_opcode == $I_HALT );


    say "FINAL: ", $final_out;
    $max_final = max( $final_out, $max_final );

} # permutations

say "DONE.";
say "MAXIMAL FINAL: ", $max_final;

