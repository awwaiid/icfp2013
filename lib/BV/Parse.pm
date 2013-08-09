package BV::Parse;

use Clone qw(clone);
use integer;

has tree => ( is => 'rw' );

has vars => ( is => 'rw' );

# Now THIS is what I call a parser. Suck it.
method parse($str) {
  $str =~ tr/()/[]/;
  $str =~ tr/ /,/;
  $str =~ s/(\w+)/'$1'/g;
  return eval($str);
  # Or the short version:
  # eval($str =~ tr/() /[],/r =~ s/(\w+)/'$1'/gr)
}

method evaluate($exp, $v) {
  $self->vars({}); # reset vars
  return $self->interp(clone($exp), $v);
}

method interp($exp, $v) {
  my $op = ref($exp) eq 'ARRAY' ? shift @$exp : $exp;
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
      return 0+$left ^ 0+$right;
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
      return (0+$right) >> 1;
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
    when('0') { 0 }
    when('1') { 1 }
    when(/^[a-z0-9_]+$/) { $self->vars->{$op} }

    default { die "UNKNOWN OP/VAL $op" }
  }
}

my $all_ops = {
  # 1 => [qw( 0 1 x )],
  2 => [qw( not shl1 shr1 shr4 shr16 )],
  3 => [qw( and or xor plus )],
  4 => [qw( if )],
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
func gen_exp($depth, $ops) {
  if($depth == 1) {
    return [qw( 0 1 x )];
  }
  my @results;
  foreach my $cost (keys %$ops) {
    if($cost <= $depth) {
      foreach my $op (@{ $ops->{$cost} }) {
        given($op) {
          when('if0') {
            my $conds = gen_exp($depth - 1, $ops);
            my $iftrues = gen_exp($depth - 1, $ops);
            my $iffalses = gen_exp($depth - 1, $ops);
            foreach my $cond (@$conds) {
              foreach my $iftrue (@$iftrues) {
                foreach my $iffalse (@$iffalses) {
                  push @results, [if => $cond, $iftrue, $iffalse];
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
          when('and') {
            my $lefts = gen_exp($depth - 1, $ops);
            my $rights = gen_exp($depth - 1, $ops);
            foreach my $left (@$lefts) {
              foreach my $right (@$rights) {
                push @results, [and => $left, $right];
              }
            }
          }
          when('or') {
            my $lefts = gen_exp($depth - 1, $ops);
            my $rights = gen_exp($depth - 1, $ops);
            foreach my $left (@$lefts) {
              foreach my $right (@$rights) {
                push @results, [or => $left, $right];
              }
            }
          }
          when('xor') {
            my $lefts = gen_exp($depth - 1, $ops);
            my $rights = gen_exp($depth - 1, $ops);
            foreach my $left (@$lefts) {
              foreach my $right (@$rights) {
                push @results, [xor => $left, $right];
              }
            }
          }
          when('plus') {
            my $lefts = gen_exp($depth - 1, $ops);
            my $rights = gen_exp($depth - 1, $ops);
            foreach my $left (@$lefts) {
              foreach my $right (@$rights) {
                push @results, [plus => $left, $right];
              }
            }
          }

          # Unary
          when('not') {
            my $rights = gen_exp($depth - 1, $ops);
            foreach my $right (@$rights) {
              push @results, [not => $right];
            }
          }
          when('shl1') {
            my $rights = gen_exp($depth - 1, $ops);
            foreach my $right (@$rights) {
              push @results, [shl1 => $right];
            }
          }
          when('shr1') {
            my $rights = gen_exp($depth - 1, $ops);
            foreach my $right (@$rights) {
              push @results, [shr1 => $right];
            }
          }
          when('shr4') {
            my $rights = gen_exp($depth - 1, $ops);
            foreach my $right (@$rights) {
              push @results, [shr4 => $right];
            }
          }
          when('shr16') {
            my $rights = gen_exp($depth - 1, $ops);
            foreach my $right (@$rights) {
              push @results, [shr16 => $right];
            }
          }
        }
      }
    }
  }

  return [ @results ];
}

1;

