use common::sense;
use warnings;
use Data::Dumper;
use List::Util qw/sum/;

my @in = (1,17,0,10,18,11,6);

for my $N (2020, 30000000) {

    my $turn = 0;
    my %nums;
    $nums{$_} = ++$turn for @in[0..$#in-1];

    my $last = $in[$#in];
    while ( $turn < $N - 1 ) {
        my $next;
        if ( $nums{$last} ) {
            $next = ($turn+1) - $nums{$last};
        } else {
            $next = 0;
        }
        $nums{$last} = ++$turn;
        #say "$turn: $last";
        $last = $next;
    }

    say $last;
}
