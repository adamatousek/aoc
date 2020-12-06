use common::sense;
use Data::Dumper;

my $counta = 0;
my $countb = 0;
while (<>)
{
    /^(\d+)-(\d+) (.): (.*)$/
        or next;
    my ($min, $max, $c, $s) = @{^CAPTURE};
    my @cs = split //, $s;
    my $occ = grep (/$c/, @cs);
    $counta++
        if $occ >= $min && $occ <= $max;
    $countb++
        if ($cs[$min-1] eq $c) + ($cs[$max-1] eq $c) == 1;
}

say $counta;
say $countb;
