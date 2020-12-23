use common::sense;
use warnings;
use Data::Dumper;
use List::Util qw/all/;

my @decks;
my @decks_b;
for my $d (0, 1) {
    while (<>) {
        last if /^$/;
        next if /Player/;
        chomp;
        push $decks[$d]->@*, $_;
        push $decks_b[$d]->@*, $_;
    }
}

# Part A
my $winner;
while (all { @$_ > 0 } @decks) {
    my @tops = map { shift @$_ } @decks;
    $winner = $tops[0] > $tops[1] ? 0 : 1;
    push $decks[$winner]->@*, ($winner ? reverse @tops : @tops);
}

say cardsum( $decks[$winner] );

# Part B

say cardsum( $decks_b[recombat( @decks_b )] );

sub recombat
{
    my @ds = @_;
    my $seen = {};
    my $winner;
    while (all { @$_ > 0 } @ds) {
        my $s = join '|', map { join ',', @$_ } @ds;

        return 0 if ($seen->{$s}++);

        my @tops = map { shift @$_ } @ds;

        if ( all { $tops[$_] <= $ds[$_]->@* } (0, 1) ) {
            $winner = recombat( map [ $ds[$_]->@[0..$tops[$_]-1] ], (0, 1) );
        } else {
            $winner = $tops[0] > $tops[1] ? 0 : 1;
        }
        push $ds[$winner]->@*, ($winner ? reverse @tops : @tops);
    }
    return $winner;
}

sub cardsum
{
    my $d = shift;
    my $sum = 0;
    my @rev = reverse @$d;
    while ( my ($i, $v) = each @rev ) {
        $sum += ($i + 1) * $v;
    }
    return $sum;
}
