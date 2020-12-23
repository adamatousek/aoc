use common::sense;
use warnings;
use List::Util qw/product/;

my $label = '614752839';

my $cup1_a = play_cups(100, 9);
say take(8, $cup1_a->{next});

my $cup1_b = play_cups(10_000_000, 1_000_000);
say product( take(2, $cup1_b->{next}) );


sub play_cups
{
    my ($rounds, $n_cups) = @_;

    my $add_more = $n_cups > length $label; # true for part B
    my @cups = map { n => $_ }, (1..$n_cups);
    my $cur = $add_more ? $cups[$#cups] : $cups[(substr $label, -1, 1) - 1];
    for my $c (split //, $label) {
        $cur->{next} = $cups[$c - 1];
        $cur = $cur->{next};
    }

    if ($add_more) {
        # $cur now points to the last cup in $label
        for my $i ((length $label) .. $#cups) {
            $cur->{next} = $cups[$i];
            $cur = $cur->{next};
        }
    }

    $cur = $cups[(substr $label, 0, 1) - 1];

    for (1..$rounds) {
        my $spl = $cur->{next};
        $cur->{next} = $cur->{next}{next}{next}{next};
        my @spl_vals = take( 3, $spl );
        my $dst = select_dst( $cur->{n}, \@cups, @spl_vals );
        $spl->{next}{next}{next} = $dst->{next};
        $dst->{next} = $spl;
        $cur = $cur->{next};
    }

    return $cups[0];
}

sub take
{
    my ($n, $src) = @_;
    return () unless $n;
    return ($src->{n}, take( $n-1, $src->{next} ));
}

sub select_dst
{
    my ($n, $cups, @excluded) = @_;
    do {
        --$n;
        $n = @$cups unless $n; #wrap
    } while grep {$_ == $n} @excluded;
    return $cups->[$n - 1];
}
