use common::sense;
use warnings;
use Data::Dumper;

$" = ',';
my @in = map [/./g], <>;
my $world = {};
while (my ($y, $row) = each @in) {
    while (my ($x, $c) = each @$row) {
        next unless $c eq '#';
        $world->{"$x,$y,0,0"} = 1;
    }
}

my @DELTAS = (-1,0,1);
my @DELTAS_A = mkdeltas ( 0 );
my @DELTAS_B = mkdeltas ( @DELTAS );

my $orig_world = { %$world };

for my $deltas (\@DELTAS_A, \@DELTAS_B) {
    for (1..6) {
        $world = step($deltas);
    }
    say scalar keys %$world;
    $world = $orig_world;
}

sub step
{
    my $deltas = shift;
    my %new;
    my %tocheck;
    for my $coord (keys %$world) {
        for my $d (@$deltas) {
            my ($dx,$dy,$dz,$dw) = @$d;
            my ($x,$y,$z,$w) = split /,/, $coord;
            my @newc = ($x + $dx, $y + $dy, $z + $dz, $w + $dw);
            $tocheck{"@newc"} = 1;
        }
    }
    for my $coord (keys %tocheck) {
        $new{$coord} = 1
            if check(split (/,/, $coord), $deltas);
    }
    return \%new;
}

sub check{
    my ($x,$y,$z,$w,$deltas) = @_;
    my $active_neighs = 0;
    for my $d (@$deltas) {
        my ($dx,$dy,$dz,$dw) = @$d;
        my @neigh = ($x + $dx, $y + $dy, $z + $dz, $w + $dw);
        ++$active_neighs if $world->{"@neigh"};
    }
    my $active = defined $world->{"$x,$y,$z,$w"};
    --$active_neighs if $active;

    if ($active) {
        return $active_neighs == 2 || $active_neighs == 3;
    } else {
        return $active_neighs == 3;
    }
}

sub mkdeltas {
    my @ws = @_;
    my @r;
    for my $dx (@DELTAS) {
        for my $dy (@DELTAS) {
            for my $dz (@DELTAS) {
                for my $dw (@ws) {
                    push @r, [$dx,$dy,$dz,$dw];
                }
            }
        }
    }
    return @r;
}
