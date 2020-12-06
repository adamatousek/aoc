use common::sense;
use warnings;

use List::Util qw/max/;

my $ra = 0;
my @b;

while (<>)
{
    chomp;
    y/FBRL/0110/;
    my $n = oct ( "0b" . $_ );
    $ra = max( $ra, $n );
    push @b, $n;
}

say $ra;

@b = sort @b;
my @tri = shift @b;

while ( @b )
{
    push ( @tri, shift @b );
    if ( $tri[0] + 2 == $tri[1] ) {
        say $tri[0] + 1;
    }
    shift @tri;
}
