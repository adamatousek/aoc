use common::sense;
use warnings;
use Data::Dumper;

my @instrs = <>;
chomp @instrs;

my $ax = 0;
my $ay = 0;
my $dir = 'E';
my %DIRS = ( E => 'S', S => 'W', W => 'N', N => 'E' );

my $bx = 0;
my $by = 0;
my $wx = 10;
my $wy = 1;


for my $inst (@instrs) {
    $inst =~ /^(\w)(\d+)/;
    my $act = $1;
    my $num = $2;
    if ( $act =~ /[LR]/ ) {
        my $steps = $num / 90;
        $steps = 4 - $steps if $act eq 'L';
        while ( $steps-- ) {
            $dir = $DIRS{ $dir };
            ($wx, $wy) = ($wy, -$wx);
        }
        next;
    }

    my @d = dir_to_d( $act eq 'F' ? $dir : $act );
    my ($dx, $dy) = map { $_ * $num } @d;
    $ax += $dx;
    $ay += $dy;

    if ( $act eq 'F' ) {
        $bx += $num * $wx;
        $by += $num * $wy;
    } else {
        $wx += $dx;
        $wy += $dy;
    }
}

say abs($ax) + abs($ay);
say abs($bx) + abs($by);

sub dir_to_d
{
    given ( $_[0] ) {
        return (0,  1) when 'N';
        return (0, -1) when 'S';
        return ( 1, 0) when 'E';
        return (-1, 0) when 'W';
    }
}
