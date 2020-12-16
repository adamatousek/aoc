use common::sense;
use warnings;
use Data::Dumper;
use List::Util qw/sum/;

my $orig = {};
my $width;
while (<>) {
    my $col = 0;
    for my $c (/./g) {
        ++$col;
        next if $c eq '.';
        $orig->{"$.;$col"} = $c eq '#';
    }
    $width = $col;
}

for my $variant (0, 1) {

    my $seats = {%$orig};
    my $changing;
    do {
        $changing = 0;
        my $new = {};
        for (keys %$seats) {
            my ($r, $c) = split /;/;
            my $seat = \$seats->{$_};
            my $occ_neighs = neighbour_stats( $seats, $r, $c, $variant );
            my $new_state;
            if ( ! $$seat && $occ_neighs == 0 ) {
                $new_state = 1;
                $changing = 1;
            } elsif ( $$seat && $occ_neighs >= (4 + $variant) ) {
                $new_state = 0;
                $changing = 1;
            } else {
                $new_state = $$seat;
            }
            $new->{$_} = $new_state;
        }
        #say Dumper $new;
        $seats = $new;
    } while $changing;

    say sum (values %$seats);
}

sub neighbour_stats
{
    my ( $s, $r, $c, $ext ) = @_;
    my $occ = 0;
    for my $dr (-1, 0, 1) {
        for my $dc (-1, 0, 1) {
            next unless $dr || $dc;
            my ( $nr, $nc ) = ( $r, $c );
            my $coords;
            do {
                $nr += $dr;
                $nc += $dc;
                $coords = "$nr;$nc";
            } while ( $ext && (not defined $s->{$coords}) && $nr > 0 && $nc > 0 && $nr <= $. && $nc <= $width );
            ++$occ if $s->{$coords};
        }
    }
    return $occ;
}
