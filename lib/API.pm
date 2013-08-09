package API;

use v5.14;
use JSON;
use LWP::UserAgent;
use Data::Printer;

=head1 NAME

API - Uses the API to get metadata, examples, and guess programs

=head1 SYNOPSIS

    use API;

    $data_struct = API::call( $path, $json_string );
    $data_struct = APP::call( $path, $perl_struct );

    $path can be 'myproblems', 'eval', 'guess', 'train', 'status'

=cut

sub call {
    my ($path, $params, $is_dry_run ) = @_;
    my $json = ref $params ? encode_json( $params ) : $params;

    open my $fh, '<', "secret.txt" or die "Can't open secret.txt - $!";
    my $auth = <$fh>;
    chomp( $auth );

    my $url = "http://icfpc2013.cloudapp.net/$path?auth=${auth}vpsH1H";

    my $ua = LWP::UserAgent->new;
    my $response = $ua->post( $url, Content => $json );
# p($response);

    return decode_json( $response->content ) unless $is_dry_run;

    say "url: $url json: $json" if $is_dry_run;
}

sub build_guess {
    my ( $id, $program ) = @_;

    return { id => $id, program => $program };
}

sub make_guess {
    my ($id, $program, $is_dry_run) = @_;

    my $content = build_guess( $id, $program );
    return unless $content;

    return call( 'guess', $content, $is_dry_run );
}


sub build_eval {
    my ( $id, $program, $args ) = @_;

    return undef if ! ( $id xor $program );

    my $struct = { arguments => $args };
    if ( $id ) {
        $struct->{id} = $id;
    }
    else {
        $struct->{program} = $program;
    }

    return $struct;
}

sub make_eval {
    my ( $id, $program, $args, $is_dry_run ) = @_;

    my $content = build_eval( $id, $program, $args );
    return unless $content;

    return call( 'eval', $content, $is_dry_run );
}

sub get_problems {
    return call( 'myproblems' );
}

sub get_pending_problems {
    my $problems = get_problems();
    my @pending = ();

    for my $prob ( @$problems ) {
        push @pending, $prob if ! $prob->{solved};
    }

    return \@pending;
}

1;
