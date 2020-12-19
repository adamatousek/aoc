use common::sense;
use warnings;

my $rules = {};
while (( my $l = <> ) =~ /(\d+): (.*)/) {
    my $rid = $1;
    my $rhs = $2;
    my @alts = split /\|/, $rhs;
    for (@alts) {
        push $rules->{$rid}->@*, [ /\d+|.(?=")/g ];
    }
}


my $b_rules = { %$rules,
                11 => [ [42,31], [42,11,31] ],
                0  => [ map [(42) x $_, 11], (1..87) ] # longest input: 88 chars
              };

my ($count_a, $count_b);
while (<>) {
    chomp;
    ++$count_a if match( $_, $rules );
    ++$count_b if match( $_, $b_rules );
}

say $count_a;
say $count_b;

sub match
{
    my ($s, $rules) = @_;
    $s = [ (split //, $s), 'X' ]; # X = end of string marker
    my $len = subm( $rules, $s, 0, 0 );
    return $len == (@$s-1);
}

sub subm
{
    my ($rs, $s, $i, $r) = @_;
    my $alts = $rs->{$r};
    if ( $alts->[0][0] =~ /[ab]/) {
        return $alts->[0][0] eq $s->[$i];
    }
    ALT: for my $alt (@$alts) {
        my $li = $i;
        for my $sub (@$alt) {
            my $matched = subm( $rs, $s, $li, $sub );
            next ALT unless $matched;
            $li += $matched;
        }
        return $li - $i;
    }
    return 0;
}

