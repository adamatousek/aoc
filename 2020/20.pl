use common::sense;
use warnings;
use List::Util qw/min all sum/;

my $W = 10;
my $tiles = {};
my $edges = {};

while (<>) {
    next if /^$/;
    my ($id) = /\d+/g;
    my @lines;
    for (1..$W) {
        push @lines, scalar <>;
    }
    chomp @lines;
    my @edges = ($lines[0]);
    push @edges, join '', map {substr $_, -1, 1} @lines;
    push @edges, $lines[$#lines];
    push @edges, join '', map {substr $_, 0, 1} @lines;
    # @edges = (top, right, bottom, left)
    my @edgeids = map { mkedge($_) } @edges;
    pop @lines;
    shift @lines;
    @lines = map {substr $_, 1, -1} @lines;
    my $tile = { id => $id, image => \@lines, edgeids => \@edgeids, edges => \@edges };
    $tiles->{$id} = $tile;
    push $edges->{$_}->@*, $tile for @edgeids;
}

my %edgetiles;
while (my ($id, $ts) = each %$edges) {
    next unless @$ts == 1;
    $edgetiles{$ts->[0]{id}}++;
}

my $a_prod = 1;
my $corner;
while (my ($id, $count) = each %edgetiles) {
    next unless $count == 2;
    $a_prod *= $id;
    $corner = $id;
}

say "A: $a_prod";


$corner = $tiles->{$corner};
while ( not topleft($corner) ) {
    rotate($corner);
}

flood_fill($corner);
my $image = combine($corner);

my @MONSTER = ( '..................#.'
              , '#....##....##....###'
              , '.#..#..#..#..#..#...'
              );

my $n_monster_hashes = count_hashes( \@MONSTER );
my $n_hashes = count_hashes( $image );


my $monsters;
ORIENTATION: for (1,2) {
    for (1..4) {
        $monsters = find_monsters($image);
        last ORIENTATION if $monsters;
        $image = rotate_img($image);
    }
    @$image = reverse @$image;
}

say 'B: ', $n_hashes - $n_monster_hashes * $monsters;

sub mkedge
{
    shift;
    y/.#/01/;
    my $x = oct "0b$_";
    my $y = oct '0b' . reverse $_;
    return min( $x, $y );
}

sub rotate_img
{
    my $img = shift;
    my @newimg;
    for my $i (0..(length $img->[0])-1) {
        push @newimg, join '', reverse (map substr($_, $i, 1), @$img);
    }
    return \@newimg;
}

sub rotate
{
    my $t = shift;
    my $newimg = rotate_img($t->{image});
    my @e = $t->{edges}->@*;
    my $tmp = pop @e;
    unshift @e, $tmp;
    $e[$_] = reverse $e[$_] for (0, 2);
    unshift $t->{edgeids}->@*, pop $t->{edgeids}->@*;
    $t->{image} = $newimg;
    $t->{edges} = \@e;
}

sub flip
{
    my $t = shift;
    $t->{image}->@* = reverse $t->{image}->@*;
    $t->{edges}[$_] = reverse $t->{edges}[$_] for (1, 3);
    for my $attr ('edges', 'edgeids') {
        my $tmp = $t->{$attr}[0];
        $t->{$attr}[0] = $t->{$attr}[2];
        $t->{$attr}[2] = $tmp;
    }
}

sub topleft
{
    my $t = shift;
    my @es = map $t->{edgeids}->[$_], (0, 3);
    return all {$edges->{$_}->@* == 1} @es;
}


sub flood_fill
{
    my $t = shift;
    my $id = $t->{id};
    my $e_bottom = $t->{edgeids}[2];
    my $e_right = $t->{edgeids}[1];
    delete $edges->{$t->{edgeids}[0]};
    delete $edges->{$t->{edgeids}[3]};

    my $t_bottom = ( grep $_->{id} != $id, $edges->{$e_bottom}->@* )[0];
    delete $edges->{$e_bottom};

    if ( $t_bottom ) {
        $t->{down} = $t_bottom;
        rotate_to_match( $t->{edges}[2], $t_bottom, 0 );
        flood_fill( $t_bottom );
    }

    my $t_right = ( grep $_->{id} != $id, $edges->{$e_right}->@* )[0];
    delete $edges->{$e_right};

    if ( $t_right ) {
        $t->{right} = $t_right;
        rotate_to_match( $t->{edges}[1], $t_right, 3 );
        flood_fill( $t_right );
    }
}

sub rotate_to_match
{
    my ($e, $t, $ei) = @_;
    for (1,2) {
        for (1..4) {
            return if ( $e eq $t->{edges}[$ei] );
            rotate( $t );
        }
        flip( $t )
    }
    die "nelicuje to";
}

sub combine
{
    my $corner = shift;
    my @tiles;
    my $t = $corner;
    while ($t) {
        my $t2 = $t;
        my @row;
        while ($t2) {
            push @row, $t2;
            $t2 = $t2->{right};
        }
        push @tiles, \@row;
        $t = $t->{down};
    }

    my @lines;
    for my $r (@tiles) {
        my $line = '';
        for my $l (0..$W-3) {
            push @lines, ( join '', map { $_->{image}[$l]} @$r )
        }
    }
    return \@lines;
}

sub find_monsters
{
    my $n = 0;
    my $img = shift;
    for my $y (0..@$img - (1+@MONSTER)) {
        for my $x (0..(length $img->[0]) - (1+length $MONSTER[0])) {
            ++$n
                if ( all { (substr $img->[$y + $_], $x, 20) =~ $MONSTER[$_]} (0..2) );
        }
    }
    return $n;
}

sub count_hashes
{
    my $img = shift;
    return sum map { scalar ( ()= /#/g ) } @$img;
}
