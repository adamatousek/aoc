use common::sense;
use warnings;
use File::Slurp qw/write_file/;

my $in = join "  , ", <>;
chomp $in;
$in =~ s/([+*])/!$1/g;

my $filename = 'tmp-18.hs';
for my $mul_prec (5, 4) {
    my $hsfile = <<~EOF;
    infixl $mul_prec !*
    (!*) = (*)
    infixl 5 !+
    (!+) = (+)

    input = [$in]

    main = print . sum \$ input
    EOF
    write_file( $filename, $hsfile );
    system( "runghc $filename" );
}
unlink $filename;
