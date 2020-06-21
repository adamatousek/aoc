#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ min max ];

my $I_ADD = 1;
my $I_MUL = 2;
my $I_IN = 3;
my $I_OUT = 4;
my $I_JNZ = 5;
my $I_JZ = 6;
my $I_LT = 7;
my $I_EQ = 8;
my $I_SRB = 9;
my $I_HALT = 99;

my $DBG = 0;

########################################################################
# INTERPRETER

sub op_arity {
    my ($bop) = @_;
    my $op = $bop % 100;
    return 3 if ( $op ~~ [$I_ADD, $I_MUL, $I_LT, $I_EQ] );
    return 2 if ( $op ~~ [$I_JNZ, $I_JZ] );
    return 1 if ( $op ~~ [$I_IN, $I_OUT, $I_SRB] );
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
    my $rb = $ctx->{rb};
    my $input = $ctx->{input};
    my $output = $ctx->{output};

    # instruction parsing state
    my $opcode;
    my $arity;
    my @mode;
    my @args;

    # helpers
    my $enlarge_mem = sub {
        my ($p) = @_;
        if ($p >= @$mem) {
            print "(enlarging to $p; " if ($DBG);
            push @$mem, ((0) x ($p - @$mem + 1));
            print 0+@$mem, ") " if ($DBG);
        }
    };
    my $load = sub {
        my ($i) = (@_, $_);
        print "Load: index ", $i if ($DBG);
        my $dst;
        given ($mode[$i]) {
            $dst = $args[$i]       when 0;
            $dst = $ip + $i + 1    when 1;
            $dst = $rb + $args[$i] when 2;
            default { die "FATAL: Unknown operand mode " . $_ }
        }
        print ", mem ", $dst if ($DBG);
        $enlarge_mem->( $dst );
        my $val = $mem->[$dst];
        say ", val ", $val if ($DBG);
        return $val;
    };

    my $store = sub {
        my ($idx, $val) = @_;
        my $dst = ($mode[$idx] == 2) ? $rb + $args[$idx] : $args[$idx];
        $enlarge_mem->( $dst );
        say "Store ", $val, " -> ", $dst if ($DBG);
        $mem->[$dst] = $val;
    };

    my $advance = sub {
        $ip += $arity + 1;
    };

    while ( ( $opcode = $mem->[$ip] ) != $I_HALT ) {
        $arity = op_arity( $opcode );
        @mode = op_mode( $opcode );
        $opcode = ($opcode % 100);
        @args = @$mem[$ip+1 .. $ip+$arity];
        say "Inst ", $mem->[$ip], ", ip ", $ip, " args" if ($DBG);
        print Dumper @args if ($DBG);

        if ( $arity == 3 ) {
            my ($lhs, $rhs) = map &$load(), (0, 1);
            my $res;
            given ( $opcode ) {
                when ( $I_ADD ) { $res = $lhs + $rhs; }
                when ( $I_MUL ) { $res = $lhs * $rhs; }
                when ( $I_LT  ) { $res = ($lhs < $rhs) ? 1 : 0; }
                when ( $I_EQ  ) { $res = ($lhs == $rhs) ? 1 : 0; }
            }
            $store->(2, $res);
        } elsif ( $arity == 2 ) {
            my ($test, $dest) = map &$load(), (0, 1);
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
                    $store->(0, $in);
                }
                when ( $I_OUT ) {
                    my $out = $load->(0);
                    push @$output, $out;
                    $advance->();
                    last;
                }
                when ( $I_SRB ) {
                    $rb += $load->(0);
                }
            }
        } else {
            die "wtf, man";
        }

        $advance->();
    }

    $ctx->{ip} = $ip;
    $ctx->{rb} = $rb;
    return $opcode
}


########################################################################
# MAIN

my @orig_mem;
while ( <> ) {
    push @orig_mem, $_ =~ /(-?\d+)/g;
}

$orig_mem[0] = 2;


my @direction = (0, -1);
my @position = (0, 0);
my $stage = 0;
my %panels;
my $minx = 0;
my $miny = 0;
my $maxx = 41;
my $maxy = 24;

paint( @position, 1 );

my @contexts;
my $ctx = 0; # Active task

{
    my @nmem = @orig_mem;
    push @contexts, { mem => \@nmem, ip => 0, rb => 0, input => [], output => [] };
}
#push @{$contexts[0]->{input}}, 2;
# 
my $x;
my $y;

my $ballx;
my $padx;

my $intr_opcode;
do {
    $intr_opcode = run_eval( $contexts[ $ctx ] );

    if ( $intr_opcode == $I_OUT ) {
        my $out = shift @{$contexts[ $ctx ]->{output}};
        #say "OUTPUT: ", $out;
        if ($stage == 0 ) {
            $x = $out;
        } elsif ($stage == 1) {
            $y = $out;
        } else {
            paint($x, $y, $out);
            $padx = $x if ($out == 3);
            $ballx = $x if ($out == 4);
        }
        $stage = ($stage + 1) % 3;
        #push @{$contexts[ ($ctx + 1) % 5 ]->{input}}, $out;
        #$final_out = $out if $ctx == 4;

        #say "CONTEXT SWITCH from ", $ctx;
        #$ctx = ($ctx + 1) % @contexts;

    } elsif ( $intr_opcode == $I_IN ) {
        #die "FATAL: Task " . $ctx . " has nothing to read";
        draw_screen();
        my $dir = $ballx <=> $padx;
        push @{$contexts[$ctx]->{input}}, $dir;

    } else {
        say "Interrupt ", $intr_opcode;
    }
} until ( $intr_opcode == $I_HALT );


say "DONE.";
say "Score: ", $panels{-1}->{0};

my $painted = 0;
foreach my $xx ( keys %panels ) {
    $painted += ($panels{$xx}->{$_} == 2 ? 1 : 0 ) for keys(%{$panels{$xx}});
}
say "Bricks: ", $painted;

sub draw_screen {
    for (my $y = $miny; $y <= $maxy; $y++) {
        for (my $x = $minx; $x <= $maxx; $x++) {
            given (colour( $x, $y )) {
                print 'X' when 1;
                print '#' when 2;
                print '=' when 3;
                print 'O' when 4;
                default { print ' ' };
            }
        }
        print "\n";
    }
}

sub colour {
    my ($x, $y) = @_;
    return 0 if not defined $panels{"$x"};
    return $panels{"$x"}->{"$y"} // 0;
}

sub paint {
    my ($x, $y, $c) = @_;
    $panels{"$x"} = {} if not defined $panels{"$x"};
    $panels{"$x"}->{"$y"} = $c;
}
