use common::sense;
use warnings;

my %als;
my %ings;
while (<>) {
    /([a-z ]*) \(contains (.*)\)$/;
    my @ings = split ' ', $1;
    my @als = split ', ', $2;
    for my $a (@als) {
        $als{$a}{_count_}++;
        for my $i (@ings) {
            $als{$a}{$i}++;
        }
        for my $i (keys $als{$a}->%*) {
            delete $als{$a}{$i} if $als{$a}{$i} != $als{$a}{_count_};
        }
   }
   for (@ings) {
       $ings{$_}{viable} = 0;
       $ings{$_}{count}++;
   }
}


my %match = reverse %{ paircheck( [keys %als], {} ) };

my $sum_a = 0;
for my $ing (values %ings) {
    next if $ing->{viable};
    $sum_a += $ing->{count};
}

say $sum_a;

say join ',', map {$match{$_}} (sort grep /^[a-z]/, keys %als);


sub paircheck {
    my ($todo,$matched) = @_;
    my @todo = @$todo;
    my $now = pop @todo;
    unless (defined $now) {
        $ings{$_}{viable} = 1 for keys %$matched;
        # Turns out it is unique and I did this recursive solution needlessly
        return $matched;
    }

    for my $i (grep /^[a-z]/, keys $als{$now}->%*) {
        next if $matched->{$i};
        my $new_matched = { %$matched, $i => $now };
        my $ret = paircheck( \@todo, $new_matched );
        return $ret if $ret;
    }
    return 0;
}
