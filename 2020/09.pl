use common::sense;
use warnings;
use Data::Dumper;
use List::Util qw/sum min max/;

my @in = <>;
chomp @in;
my @orig = @in;

my $window = 25;
my @window = splice @in, 0, $window;
my $sum;

while ( @in )
{
    my $x = shift @in;
    unless ( viable( $x ) ) {
        say "A: $x";
        $sum = $x;
        last;
    }
    shift @window;
    push @window, $x;
}

my $l = 0;
my $u = 0;
while ( 1 )
{
    my $s = sum @orig[$l..$u];
    if ( $s == $sum ) {
        my $min = min @orig[$l..$u];
        my $max = max @orig[$l..$u];
        say "B: ", $min + $max;
        last;
    } elsif ( $s > $sum ) {
        ++$l;
        $u = $l if $u < $l;
    } elsif ( $s < $sum ) {
        ++$u;
    }
}



sub viable
{
    my $x = shift;
    for my $i (0..$#window-1) {
        for my $j ($i+1..$#window) {
            return 1 if $x == $window[$i] + $window[$j];
        }
    }
    return 0;
}
