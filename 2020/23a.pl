use common::sense;
use warnings;
use Data::Dumper;
use List::Util qw/all/;

my $label = '614752839';
my $rounds = 100;

my $QUIET = 1;

#$label = '389125467';
#$rounds = 10;

my @cups = split //, $label;

for (1..$rounds) {
    say "==== Round $_ ====" unless $QUIET;
    say 'cups: ', @cups unless $QUIET;
    my @sel = splice @cups, 1, 3;
    say 'sel: ', @sel unless $QUIET;
    my $dst = find_dst( $cups[0] );
    say 'dst: ', $dst, ' -> ', $cups[$dst] unless $QUIET;
    splice @cups, $dst + 1, 0, @sel;
    push @cups, shift @cups;
}

push @cups, shift @cups until $cups[0] == 1;
say @cups[1..$#cups];

sub ix { $_[0] % @cups }
sub decr_val { $_ = shift; --$_; $_ ? $_ : 9 }

sub find_dst
{
    my $curv = shift;
    my %cupvals = reverse %cups[0..5];
    my $dstv = decr_val( $curv );
    $dstv = decr_val( $dstv ) until defined $cupvals{$dstv};
    return $cupvals{$dstv};
}
