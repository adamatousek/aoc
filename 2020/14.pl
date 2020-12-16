use common::sense;
use warnings;
use Data::Dumper;
use List::Util qw/sum/;

my @cmds = <>;
chomp @cmds;

my $mask;
my %mem_a;
my %mem_b;
for (@cmds)
{
    if ( /mask = (.*)/ ) {
        $mask = $1;
    } elsif ( /mem\[(\d+)\] = (\d+)/ ) {
        $mem_a{$1} = masked( $mask, $2 );
        $mem_b{$_} = $2 for fluctuating( $mask, $1 );
    } else {
        die 'unexpected line: ' . $_;
    }
}

say sum( values %mem_a );
say sum( values %mem_b );

sub masked
{
    my ($m, $n) = @_;
    no warnings 'portable';
    $n |= oct '0b' . ($m =~ y/X/0/r);
    $n &= oct '0b' . ($m =~ y/X/1/r);
    return $n;
}

sub fluctuating
{
    my ($m, $n) = @_;
    no warnings 'portable';
    $n |= oct '0b' . ($m =~ y/X/0/r);
    $m =~ y/X01/FXX/;
    my @r;
    my $Fcount =()= $m =~ /F/g;
    for my $p (0..(1 << $Fcount) - 1) {
        my $n1 = '';
        for my $c (split //, $m) {
            if ($c eq 'X') {
                $n1 .= 'X';
            } else {
                $n1 .= $p % 2;
                $p >>= 1;
            }
        }
        push @r, masked( $n1, $n );
    }
    return @r;
}
