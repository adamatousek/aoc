use common::sense;
use warnings;

my @text = <>;
chomp @text;

my $last_countdown = -1;

while ( 1 ) {
    my $acc = 0;
    my $ip = 0;
    my %vis;
    my $countdown = $last_countdown;

    until ( $ip > $#text || $vis{$ip} ) {
        ++$vis{$ip};
        my ($inst, $arg) = split / /, $text[$ip];

        $acc += $arg if ( $inst eq 'acc' );

        if ( $inst =~ /jmp|nop/ && $countdown-- == 0 ) {
            if ( $inst eq 'jmp' ) {
                $inst = 'nop';
            } else {
                $inst = 'jmp';
            }
        }

        if ( $inst eq 'jmp' ) {
            $ip += $arg;
        } else {
            ++$ip;
        }
    }

    say 'A: ', $acc unless ++$last_countdown;

    if ( $ip > $#text ) {
        say 'B: ', $acc;
        last;
    }
}

