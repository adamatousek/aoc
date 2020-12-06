use common::sense;
use warnings;

use Data::Dumper;

my $ra = 0;
my $rb = 0;

my %g;
my $gsize = 0;

while (<>)
{
    if ( /^$/ ) {
        commit_group();
        next;
    }
    $g{$_} += 1 for /./g;
    ++$gsize;
}
commit_group();

say $ra;
say $rb;

sub commit_group
{
    $ra += keys %g;
    for my $k ( keys %g ) {
        ++$rb
            if ( $g{$k} == $gsize );
    }
    %g = ();
    $gsize = 0;
}
