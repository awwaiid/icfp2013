package BV::Parse;

use Data::Dumper;
use Clone qw(clone);
use integer;
use Math::Int64 qw( uint64 uint64_rand );

has tree => ( is => 'rw' );

has vars => ( is => 'rw' );

# Now THIS is what I call a parser. Suck it.
method parse($str) {
  $str =~ tr/()/[]/;
  $str =~ tr/ /,/;
  $str =~ s/(\w+)/'$1'/g;
  # say "parsed: $str\n";
  my $v = eval($str);
  if($@) {
    die "ERROR: $@";
  }
  return $v;
  # Or the short version:
  # eval($str =~ tr/() /[],/r =~ s/(\w+)/'$1'/gr)
}

method evaluate($exp, $v) {
  $self->vars({}); # reset vars
  my $v64 = uint64($v);
  # printf "v64: 0x%016X (%s)\n", $v64, ref($v64);
  # say "exp: " . Dumper($exp);
  # return $self->interp(clone($exp), $v);
  my $out = $self->interp(clone($exp), $v64);
  # print "interp result: " . Dumper($out);
  # printf "interp v64: 0x%016X\n", $out;
  # printf "interp v64 str: 0x%s\n", Math::Int64::uint64_to_hex($out);
  return $out;
}

  $| = 1;
method interp($exp, $v) {
  # say "here!";
  my $op = ref($exp) eq 'ARRAY' ? shift @$exp : $exp;
  # say "interp(" . Dumper($exp) . ")";
  # use Carp;
  # Carp::cluck 'hmm' unless defined $op;
  given($op) {

    when('lambda') {
      my $varname = (shift @$exp)->[0];
      $self->vars->{$varname} = $v;
      return $self->interp(shift @$exp);
    }

    when('if0') {
      my $cond = $self->interp(shift @$exp);
      if($cond == 0) {
        return $self->interp( $exp->[0] );
      } else {
        return $self->interp( $exp->[1] );
      }
    }

    when('fold') {
      # (fold e0 e1 (lambda (x y) e2))
      my $e0 = $self->interp(shift @$exp);
      my $e1 = $self->interp(shift @$exp);
      my $lambda = shift @$exp;
      die "lambda expected!" unless shift @$lambda eq 'lambda';
      my ($x, $y) = @{ shift @$lambda };
      my $e2 = shift @$lambda;
      my $y_val = $e1;
      for(1..8) {
        my $x_val = $e0 & 0xFF; # Grab just the rightmost byte
        $e0 = $e0 >> 8;         # Shift over to get rid of that byte
        $self->vars->{$x} = $x_val;
        $self->vars->{$y} = $y_val;
        $y_val = $self->interp(clone($e2));
      }
      return $y_val;
    }

    # Binary
    when('and') {
      my $left = $self->interp(shift @$exp);
      my $right = $self->interp(shift @$exp);
      return 0+$left & 0+$right;
    }
    when('or') {
      my $left = $self->interp(shift @$exp);
      my $right = $self->interp(shift @$exp);
      return 0+$left | 0+$right;
    }
    when('xor') {
      my $left = $self->interp(shift @$exp);
      my $right = $self->interp(shift @$exp);
      return 0+$left ^ 0+$right;
    }
    when('plus') {
      my $left = $self->interp(shift @$exp);
      my $right = $self->interp(shift @$exp);
      return 0+$left + 0+$right;
    }

    # Unary
    when('not') {
      my $right = $self->interp(shift @$exp);
      return ~ (0+$right);
    }
    when('shl1') {
      my $right = $self->interp(shift @$exp);
      return (0+$right) << 1;
    }
    when('shr1') {
      my $right = $self->interp(shift @$exp);
      return ($right) >> 1;
    }
    when('shr4') {
      my $right = $self->interp(shift @$exp);
      return (0+$right) >> 4;
    }
    when('shr16') {
      my $right = $self->interp(shift @$exp);
      return (0+$right) >> 16;
    }

    # Simple constants and vars
    # when('0') { 0 }
    # when('1') { 1 }
    when('0') { uint64(0) }
    when('1') { uint64(1) }
    when(/^[a-z0-9_]+$/) { $self->vars->{$op} }

    default { die "UNKNOWN OP/VAL $op" }
  }
}

my $all_ops = {
  # 1 => [qw( 0 1 x )],
  2 => [qw( not shl1 shr1 shr4 shr16 )],
  3 => [qw( and or xor plus )],
  4 => [qw( if0 )],
  5 => [qw( fold )],
};

func limit_ops($ops_list) {
  # push @$ops_list, qw( 0 1 x );
  my $ok_ops = {};
  my (%in_ops_list) = map { ($_ => 1) } @$ops_list;
  foreach my $cost (keys %$all_ops) {
    $ok_ops->{$cost} = [
      grep { $in_ops_list{$_} } @{ $all_ops->{$cost} }
    ]
  }
  return $ok_ops;
}

# $ops = {
#   min_cost1 => [ opA opB opC ],
#   min_cost2 => [ opD opE opF ],
# }

func gen_exp($max_cost, $ops) {
  # say "gen_exp($max_cost)";
  my @results;
  push @results, [1, 0];
  push @results, [1, 1];
  push @results, [1, 'x'];
  foreach my $cost (keys %$ops) {
    if($cost <= $max_cost) {
      foreach my $op (@{ $ops->{$cost} }) {
        given($op) {
          when('if0') {
            my $conds = gen_exp($max_cost - 3, $ops);
            foreach my $cond (@$conds) {
              # my $cond = clone $cond;
              my $cond_cost = $cond->[0];
              my $iftrues = gen_exp($max_cost - $cond_cost - 2, $ops);
              foreach my $iftrue (@$iftrues) {
                # my $iftrue = clone $iftrue;
                my $iftrue_cost = $iftrue->[0];
                my $iffalses = gen_exp($max_cost - $cond_cost - $iftrue_cost - 1, $ops);
                foreach my $iffalse (@$iffalses) {
                  my $iffalse_cost = $iffalse->[0];
                  push @results, [
                    (1 + $cond_cost + $iftrue_cost + $iffalse_cost),
                    [if0 => $cond->[1], $iftrue->[1], $iffalse->[1]]];
                }
              }
            }
          }

          # when('fold') {
            # # (fold e0 e1 (lambda (x y) e2))
            # my $e0 = $self->interp(shift @$exp);
            # my $e1 = $self->interp(shift @$exp);
            # my $lambda = shift @$exp;
            # die "lambda expected!" unless shift @$lambda eq 'lambda';
            # my ($x, $y) = @{ shift @$lambda };
            # my $e2 = shift @$lambda;
            # my $y_val = $e1;
            # for(1..8) {
              # my $x_val = $e0 & 0xFF; # Grab just the rightmost byte
              # $e0 = $e0 >> 8;         # Shift over to get rid of that byte
              # $self->vars->{$x} = $x_val;
              # $self->vars->{$y} = $y_val;
              # $y_val = $self->interp(clone($e2));
            # }
            # return $y_val;
          # }

          # Binary
          when(/and|or|xor|plus/) {
            my $lefts = gen_exp($max_cost - 2, $ops);
            foreach my $left (@$lefts) {
              # my $left = clone($left);
              my $left_cost = $left->[0];
              my $rights = gen_exp($max_cost - $left_cost - 1, $ops);
              # say "rights: " . Dumper($rights);
              foreach my $right (@$rights) {
                # my $right = clone($right);
                # say "right: " . Dumper($right);
                my $right_cost = $right->[0];
                push @results, [
                  ($left_cost + $right_cost + 1),
                  [$op => $left->[1], $right->[1]]];
              }
            }
          }

          # Unary
          when(/not|shl1|shr1|shr4|shr16/) {
            my $rights = gen_exp($max_cost - 1, $ops);
            foreach my $right (@$rights) {
              my $right_cost = $right->[0];
              push @results, [
                ($right_cost + 1),
                [$op => $right->[1]]];
            }
          }
        }
      }
    }
  }

  # say "gen_exp($max_cost) -> " . Dumper(\@results);
  return [ @results ];
}

func render($exp) {
  if(! ref($exp)) {
    return $exp;
  } elsif( scalar @$exp == 1) {
    return $exp->[0];
  } else {
    return '(' . join(' ', map { render($_) } @$exp) . ')';
  }
}

func generate($cost, $ops) {
  my $ops_struct = limit_ops($ops);
  my $combos = gen_exp($cost - 1, $ops_struct);
  # say "Combos: " . Dumper($combos);
  return [
    map { render(['lambda (x)', $_ ]) }
    map { $_->[1] }
    grep { $_->[0] == ($cost - 1) }
    @$combos
  ];
}

use Memoize;
use Storable qw( nfreeze );
sub gen_exp_normalize {
  return $_[0];
  # say "dump: " . Dumper([@_]);
  # my $f = Dumper([@_]);
  # say STDERR "norm: $f";
  # return $f;
}
# memoize('gen_exp',
  # NORMALIZER => 'gen_exp_normalize',
# );

1;

