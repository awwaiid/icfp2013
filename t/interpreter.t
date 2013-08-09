
use Test::More;
use lib 'lib';
use ICFP;
use BV::Parse;

my $parser = BV::Parse->new;

my $simple = $parser->parse("(lambda (x) x)");
is_deeply $simple, [lambda => ['x'], 'x'], 'Simple lambda';
for my $n (1..10) {
  is $parser->evaluate($simple, $n), $n, "simple $n -> $n";
}

done_testing();

