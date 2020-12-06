use common::sense;
use List::Util qw/none/;

my $valid_count = 0;
my $valid_count_b = 0;
my @p;

while (<>)
{
    chomp;
    if ( /^$/ ) {
        p_done();
        next;
    }
    push @p, split /[: ]/;
}
p_done();

say $valid_count;
say $valid_count_b;


sub p_done
{
    my %p = @p;
    @p = ();

    my @ks = keys %p;
    ++$valid_count
        if (@ks == 8 || ( @ks == 7 && none {/^cid$/} @ks));

    return unless @ks >= 7;
    return unless $p{byr} =~ /^\d{4}$/ && $p{byr} >= 1920 && $p{byr} <= 2002;
    return unless $p{iyr} =~ /^\d{4}$/ && $p{iyr} >= 2010 && $p{iyr} <= 2020;
    return unless $p{eyr} =~ /^\d{4}$/ && $p{eyr} >= 2020 && $p{eyr} <= 2030;
    return unless $p{hcl} =~ /^#[0-9a-f]{6}$/;
    return unless $p{ecl} ~~ [ qw/amb blu brn gry grn hzl oth/ ];
    return unless $p{pid} =~ /^\d{9}$/;
    return unless $p{hgt} =~ /^(\d{2,3})(cm|in)$/ && (
        ($2 eq 'cm' && $1 >= 150 && $1 <= 193) ||
        ($2 eq 'in' && $1 >= 59  && $1 <= 76) );

    ++$valid_count_b;

}
