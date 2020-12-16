use common::sense;
use warnings;
use Data::Dumper;

my %bags;

while (<>) {
    /^(.*) bags contain/;
    my $b = $1;
    foreach (/\d+ [a-z ]*(?= bag)/g) {
        /(\d+) (.*)/;
        my $subbag = $2;
        my $amount = $1;
        $bags{$b}->{contain}->{$subbag} = $amount;
        $bags{$subbag}->{inside}->{$b} = 1;
    }
}

my %topbags;

search( "shiny gold" );

say scalar keys %topbags;

say countsub( "shiny gold" ) - 1;

sub search
{
    my $b = shift;
    for my $sup ( keys $bags{$b}->{inside}->%* ) {
        next if $topbags{$sup};
        $topbags{$sup} = 1;
        search( $sup );
    }
}

sub countsub
{
    my $b = shift;
    my $c = 1;
    my $subs = $bags{$b}->{contain};
    for my $sub ( keys $subs->%* ) {
        $c += $subs->{$sub} * countsub( $sub );
    }
    return $c;
}
