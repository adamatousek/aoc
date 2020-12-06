use common::sense;

my @lines = map {chomp; [split //]} grep /./, <>;

my $prod = 1;

foreach ( "3,1", "1,1", "5,1", "7,1", "1,2" ) {
    my ($dx, $dy) = split /,/;
    my $trees = 0;
    my $row = 0;
    my $col = 0;
    while ( $row < @lines )
    {
        if ( $lines[ $row ]->[ $col ] eq '#' ) {
            $trees++;
        }
        $col = ($col + $dx) % $lines[ $row ]->@*;
        $row += $dy;
    }
    say $trees
        if $prod == 1;
    $prod *= $trees;
}

say $prod;
