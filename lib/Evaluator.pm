package Evaluator;

use v5.14;
use API;
use JSON;
use LWP::UserAgent;
use Try::Tiny;
use Data::Printer;
use BV::Parse;

sub eliminate_the_weak {
    my (
        $program_id,
        $inputs,
        $solutions,
        $is_dry_run,
    ) = @_;

    my $response = API::make_eval( $program_id, undef, $inputs, $is_dry_run);
    return undef if ! $response || ! ref($response) eq 'HASH' || $response->{status} eq 'error';

    # my $not_weak = BV::Parse::treeify_lookup($solutions, $response->{outputs});
    my $not_weak = $solutions->{ join(",", @{ $response->{outputs} }) };
    return $not_weak;
}

1;
