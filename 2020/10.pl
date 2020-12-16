use common::sense;
use warnings;
use Data::Dumper;
use List::Util qw/product/;

my @in = (0, sort { $a <=> $b } <>);
chomp @in;
push @in, $in[-1] + 3;

my @d;
my @oneruns;
my $ones = 0;

for my $i (0..$#in-1) {
    my $d = $in[$i+1] - $in[$i];
    ++$d[$d];
    if ( $d == 1 ) { # no twos in the input
        ++$ones;
    } else {
        push @oneruns, $ones if $ones;
        $ones = 0;
    }
}


say Dumper \@oneruns;
say Dumper \@in;
say Dumper \@d;

say "A: ", $d[1] * $d[3];
die "Fuck." if $d[0] || $d[2];
say "B: ", product(map { onepaths( $_ ) } @oneruns);

sub onepaths
{
    my $n = shift;
    ++$n;
    # jebat
    return 1 if $n < 3;
    return 2 if $n == 3;
    return 4 if $n == 4;
    return 7 if $n == 5;
    die "Fuck!";
}
