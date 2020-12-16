use common::sense;
use warnings;
use List::Util qw/any/;

my @ranges;
my %fields;

while ( (my $l = <>) =~ /:/ ) {
    $l =~ /(.*):/;
    my $f = $1;
    my @bounds = $l =~ /\d+/g;
    my @ran;
    push @ran, [splice @bounds, 0, 2] while @bounds;
    push @ranges, @ran;
    $fields{$f} = \@ran;
}

my @in = map {[/\d+/g]} <>;

each @in;
my (undef, $my_t) = each @in;

my $sum_a = 0;

each @in;
$in[0] = $my_t;

my @valid;
while ( my (undef, $l) = each @in ) {
    my $s = suminval( $l );
    $sum_a += $s;
    push @valid, $l if $s == 0;
}

say $sum_a;

my @viable;
push @viable, { map { $_ => 1 } keys %fields } for (1..@$my_t);

for my $t (@valid) {
    while (my ($i, $n) = each @$t) {
        for my $f (keys $viable[$i]->%*) {
            unless ( any {$n >= $_->[0] && $n <= $_->[1]} $fields{$f}->@* ) {
                delete $viable[$i]->{$f};
            }
        }
    }
}

while (any { keys %$_ > 1 } @viable) {
    my @certain = map { keys %$_ } grep { keys %$_ == 1 } @viable;
    for my $v (grep { keys %$_ > 1 } @viable) {
        delete $v->{$_} for @certain;
    }
}

my $prod_b = 1;
while (my ($i, $h) = each @viable) {
    $prod_b *= $my_t->[$i] if (keys %$h)[0] =~ /^departure/;
}

say $prod_b;


sub suminval
{
    my $t = shift;
    my $s = 0;
    for my $n (@$t) {
        $s += $n if inval( $n );
    }
    return $s;
}

sub inval
{
    my $n = shift;
    for my $r (@ranges) {
        return 0 if $n >= $r->[0] && $n <= $r->[1];
    }
    return 1;
}
