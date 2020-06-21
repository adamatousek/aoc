#!/usr/bin/perl
use v5.16;
use strict;
use warnings;
no warnings 'experimental';
use Data::Dumper;
use List::Util qw[ min max ];

my $WALL = -1;
my $UNDISC = -2;
my $INF = 99999;

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
                    #say "INPUT: ", $in;
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

my %panels;
my $minx = 0;
my $miny = 0;
my $maxx = 0;
my $maxy = 0;
my $steps = 0;
my @stack = (0);
my $dist;
my @oxy;

my $x = 0;
my $y = 0;

my $prog = <<EOF
NOT A J
NOT T T
AND B T
AND C T
NOT T T
OR T J
AND D J
NOT E T
NOT T T
OR H T
AND T J
RUN
EOF
;

my @in = $prog =~ /./gs;
@in = map {ord $_} @in;

my @contexts;
my $ctx = 0; # Active task

my @nmem = @orig_mem;
push @contexts, { mem => \@nmem, ip => 0, rb => 0, input => [], output => [] };
push @{$contexts[0]->{input}}, @in;

my $intr_opcode;
do {
    $intr_opcode = run_eval( $contexts[ $ctx ] );

    if ( $intr_opcode == $I_OUT ) {
        my $out = shift @{$contexts[ $ctx ]->{output}};
        if ( $out < 127 ) {
            print chr($out);
        } else {
            print "[$out]";
        }
    } elsif ( $intr_opcode == $I_IN ) {
        die "FATAL: Task " . $ctx . " has nothing to read";
    } else {
        say "Interrupt ", $intr_opcode;
    }
} until ( $intr_opcode == $I_HALT or not @stack);

say "DONE.";

sub draw {
    for (my $y = $miny; $y <= $maxy; $y++) {
        for (my $x = $minx; $x <= $maxx; $x++) {
            print chr(colour( $x, $y ));
        }
        #print "\n";
    }
    say "-" x 40;
}

sub revdir {
    my $d = shift;
    return $d + 1 if $d % 2 == 1;
    return $d - 1;
}

sub colour {
    my ($x, $y) = @_;
    return ord('!') if not defined $panels{"$x"};
    return $panels{"$x"}->{"$y"} // ord('!');
}

sub paint {
    my ($x, $y, $c) = @_;
    $panels{"$x"} = {} if not defined $panels{"$x"};
    $panels{"$x"}->{"$y"} = $c;

    $minx = min $minx, $x;
    $miny = min $miny, $y;
    $maxx = max $maxx, $x;
    $maxy = max $maxy, $y;
}

sub addv {
    my ($v1, $v2) = @_;
    my @v3 = @$v1;
    $v3[$_] += $v2->[$_] for (0..1);
    return @v3;
}

