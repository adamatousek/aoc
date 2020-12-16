use common::sense;
use warnings;
use Data::Dumper;
use List::Util qw/max/;
use Math::Prime::Util qw/chinese/;

my $scaler = 10000;

my ($start, $ps) = <>;
chomp $start;
chomp $ps;
my @ps = $ps =~ /\d+/g;

my %fracts = map { ( $scaler * $start / $_ ) % $scaler => $_ } @ps;
my $soonest_id = $fracts{ max( keys %fracts ) };
my $wait = $soonest_id - $start % $soonest_id;

say $soonest_id * $wait;

## Part 2

@ps = split /,/, $ps;

my $m = 0;
my @congs;

for my $p (@ps)
{
    next if $p eq 'x';
    push @congs, { modulus => $p, eq => ($p - $m) % $p };
} continue { ++$m; }

say chinese( map [$_->{eq}, $_->{modulus}], @congs );
