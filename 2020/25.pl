use common::sense;
use warnings;
use Math::Prime::Util qw/mulmod powmod/;
use constant { M => 20201227 # modulus
             , G => 7        # root
             };

my @keys = <>;
chomp @keys;

my $x = 1;
my $gx = G;
while ( $gx != $keys[0] ) {
    $gx = mulmod($gx, G, M);
    ++$x;
}

say powmod( $keys[1], $x, M);
