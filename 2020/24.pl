use common::sense;
use warnings;
no warnings 'experimental';

my %tiles;

while (<>) {
    my @path = /[ns]?[ew]/g;
    my ($x, $y);
    for (@path) {
        given ($_) {
            $x += 2 when 'e';
            $x -= 2 when 'w';
            $x += 1, $y += 1 when 'ne';
            $x += 1, $y -= 1 when 'se';
            $x -= 1, $y += 1 when 'nw';
            $x -= 1, $y -= 1 when 'sw';
        }
    }
    $tiles{"$x,$y"}++;
}

map { delete $tiles{$_} unless $tiles{$_} % 2 } keys %tiles;
say scalar keys %tiles;

my $tiles = \%tiles;
for (1..100) {
    my %todo;
    for my $t (keys %$tiles) {
        $todo{$_} = 1 for neighbours($t);
    }
    my %new;
    for my $t (keys %todo) {
        my $black_neighbours = grep { defined $tiles->{$_} } neighbours($t);
        $new{$t} = 1 if (!$tiles->{$t} && $black_neighbours == 2);
        $new{$t} = 1 if ( $tiles->{$t} && $black_neighbours > 0 && $black_neighbours <= 2);
    }
    $tiles = \%new;
}

say scalar keys %$tiles;

sub neighbours
{
    my $c = shift;
    my ($x, $y) = split /,/, $c;
    my @ns;
    for my $dx (-1,1) {
        for my $dy (-1,1) {
            push @ns, join ',', ($x + $dx, $y + $dy);
        }
        push @ns, join ',', ($x + 2 * $dx, $y);
    }
    return @ns;
}
