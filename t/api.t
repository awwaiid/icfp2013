use Test::More;
use lib 'lib';
use ICFP;
use API;

my $guess_struct = API::build_guess( '123', "(lambda (x_316) (not x_316))");
my $expected_guess = { id => '123', program => "(lambda (x_316) (not x_316))", };
is_deeply $guess_struct, $expected_guess, "Built a guess with id 123 and size 3";

my $eval_struct = API::build_eval('123', undef, [ 123, 456 ] );
my $expected_eval = { id => 123, arguments => [ 123, 456 ], };
is_deeply $eval_struct, $expected_eval, "Built eval with id 123 and args [123,456]";

$eval_struct = API::build_eval(undef,  "(lambda (x_316) (not x_316))", [ 123, 456 ] );
my $expected_eval = { program => "(lambda (x_316) (not x_316))", arguments => [ 123, 456 ], };
is_deeply $eval_struct, $expected_eval, "Built eval with program (lambda (x_316) (not x_316)) and args [123,456]";

$eval_struct = API::build_eval(undef, undef, [ 123, 456 ] );
is $eval_struct, undef, "No struct returned when both id and program are undef";

$eval_struct = API::build_eval(123, "(lambda (x_316) (not x_316))", [ 123, 456 ] );
is $eval_struct, undef, "No struct returned when both id and program are set to something";


done_testing();

