package BV::Parse;

# use v5.14;
# use Moo;
# use Method::Signatures::Simple;

no warnings 'recursion';
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

  my $out = interp(clone($exp), {}, $v64);

  # my $code = compile(clone($exp));
  # my $f = eval($code);
  # my $out = $f->($v64);

  return $out;
}

  $| = 1;
  my $int64_zero = uint64(0);
  my $int64_one  = uint64(1);
func interp($exp, $vars, $v) {
  # say "here!";
  my $op = ref($exp) eq 'ARRAY' ? shift @$exp : $exp;
  # say "interp(" . Dumper($exp) . ")";
  # use Carp;
  # Carp::cluck 'hmm' unless defined $op;
  given($op) {

    # Simple constants and vars
    # when('0') { uint64(0) }
    # when('1') { uint64(1) }
    # when('0') { 0 }
    # when('1') { 1 }
    when('0') { $int64_zero }
    when('1') { $int64_one  }

    # Unary
    when('not') {
      my $right = interp(shift @$exp,$vars);
      return ~ $right;
    }
    when('shl1') {
      my $right = interp(shift @$exp,$vars);
      return $right << 1;
    }
    when('shr1') {
      my $right = interp(shift @$exp,$vars);
      return $right >> 1;
    }
    when('shr4') {
      my $right = interp(shift @$exp,$vars);
      return $right >> 4;
    }
    when('shr16') {
      my $right = interp(shift @$exp,$vars);
      return $right >> 16;
    }

    # Binary
    when('and') {
      my $left = interp(shift @$exp,$vars);
      my $right = interp(shift @$exp,$vars);
      return $left & $right;
    }
    when('or') {
      my $left = interp(shift @$exp,$vars);
      my $right = interp(shift @$exp,$vars);
      return $left | $right;
    }
    when('xor') {
      my $left = interp(shift @$exp,$vars);
      my $right = interp(shift @$exp,$vars);
      return $left ^ $right;
    }
    when('plus') {
      my $left = interp(shift @$exp,$vars);
      my $right = interp(shift @$exp,$vars);
      return $left + $right;
    }

    when('if0') {
      my $cond = interp(shift @$exp,$vars);
      if($cond == 0) {
        return interp( $exp->[0] ,$vars);
      } else {
        return interp( $exp->[1] ,$vars);
      }
    }

    when('fold') {
      # (fold e0 e1 (lambda (x y) e2))
      my $e0 = interp(shift @$exp,$vars);
      my $e1 = interp(shift @$exp,$vars);
      my $lambda = shift @$exp;
      die "lambda expected!" unless shift @$lambda eq 'lambda';
      my ($x, $y) = @{ shift @$lambda };
      my $e2 = shift @$lambda;
      my $y_val = $e1;
      for(1..8) {
        my $x_val = $e0 & 0xFF; # Grab just the rightmost byte
        $e0 = $e0 >> 8;         # Shift over to get rid of that byte
        $vars->{$x} = $x_val;
        $vars->{$y} = $y_val;
        $y_val = interp(clone($e2),$vars);
      }
      return $y_val;
    }

    when('lambda') {
      my $varname = (shift @$exp)->[0];
      $vars->{$varname} = $v;
      return interp(shift @$exp,$vars);
    }

    when(/^[a-z0-9_]+$/) { $vars->{$op} }

    default { die "UNKNOWN OP/VAL $op" }
  }
}

func compile($exp) {
  # say "here!";
  my $op = ref($exp) eq 'ARRAY' ? shift @$exp : $exp;
  # say "interp(" . Dumper($exp) . ")";
  # use Carp;
  # Carp::cluck 'hmm' unless defined $op;
  given($op) {

    when('lambda') {
      my $varname = (shift @$exp)->[0];
      my $body = compile(shift @$exp);
      return qq|
        sub {
          my \$$varname = shift;
          $body
        }
      |;
    }

    when('if0') {
      my $cond = compile(shift @$exp);
      my $truepart = compile(shift @$exp);
      my $falsepart = compile(shift @$exp);
      return qq|
        (($cond == 0) ? ($truepart) : ($falsepart))
      |;
    }

    when('fold') {
      # (fold e0 e1 (lambda (y z) e2))
      my $e0 = compile(shift @$exp);
      my $e1 = compile(shift @$exp);
      my $lambda = shift @$exp;
      die "lambda expected!" unless shift @$lambda eq 'lambda';
      my ($y_var, $z_var) = @{ shift @$lambda };
      my $e2 = compile(shift @$lambda);
      return qq|
        (
          sub {
            my \$f = $e0;
            my \$$z_var = $e1;
            for(1..8) {
              my \$$y_var = \$f & 0xFF; # Grab just the rightmost byte
              \$f = \$f >> 8;
              \$$z_var = $e2;
            }
            return \$$z_var;
          }
        )->()
      |;
    }

    # Binary
    when('and') {
      my $left  = compile(shift @$exp);
      my $right = compile(shift @$exp);
      return qq|
        (($left) & ($right))
      |;
    }
    when('or') {
      my $left  = compile(shift @$exp);
      my $right = compile(shift @$exp);
      return qq{
        (($left) | ($right))
      };
    }
    when('xor') {
      my $left  = compile(shift @$exp);
      my $right = compile(shift @$exp);
      return qq|
        (($left) ^ ($right))
      |;
    }
    when('plus') {
      my $left  = compile(shift @$exp);
      my $right = compile(shift @$exp);
      return qq|
        (($left) + ($right))
      |;
    }

    # Unary
    when('not') {
      my $right = compile(shift @$exp);
      return qq| (~ ($right)) |;
    }
    when('shl1') {
      my $right = compile(shift @$exp);
      return qq| (($right) << 1) |;
    }
    when('shr1') {
      my $right = compile(shift @$exp);
      return qq| (($right) >> 1) |;
    }
    when('shr4') {
      my $right = compile(shift @$exp);
      return qq| (($right) >> 4) |;
    }
    when('shr16') {
      my $right = compile(shift @$exp);
      return qq| (($right) >> 16) |;
    }

    # Simple constants and vars
    when('0') { "uint64(0)" }
    when('1') { "uint64(1)" }
    when(/^[a-z0-9_]+$/) { "\$$op" }

    default { die "UNKNOWN OP/VAL $op" }
  }
}

my $all_ops = {
  # 1 => [qw( 0 1 x )],
  2 => [qw( not shl1 shr1 shr4 shr16 )],
  3 => [qw( and or xor plus )],
  4 => [qw( if0 )],
  5 => [qw( fold tfold )],
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

my $c = 0;
my $is_fold = 0;
my $is_tfold = 0;
func gen_exp($max_cost, $ops) {
  print "." unless $c++ % 10000;
  # say "gen_exp($max_cost)";
  my @results;
  push @results, [1, 0];
  push @results, [1, 1];
  push @results, [1, 'x'];
  push @results, [1, 'y'] if $is_fold || $is_tfold;
  push @results, [1, 'z'] if $is_fold;
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
            # my $e0 = interp(shift @$exp);
            # my $e1 = interp(shift @$exp);
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
              # $y_val = interp(clone($e2));
            # }
            # return $y_val;
          # }

          # (fold e0 e1 (lambda (y z) e2))
          when('fold') {
            my $e0s = gen_exp($max_cost - 4, $ops);
            foreach my $e0 (@$e0s) {
              my $e0_cost = $e0->[0];
              my $e1s = gen_exp($max_cost - $e0_cost - 3, $ops);
              foreach my $e1 (@$e1s) {
                # my $iftrue = clone $iftrue;
                my $e1_cost = $e1->[0];
                $is_fold = 1;
                my $e2s = gen_exp($max_cost - $e0_cost - $e1_cost - 2, $ops);
                $is_fold = 0;
                foreach my $e2 (@$e2s) {
                  my $e2_cost = $e2->[0];
                  push @results, [
                    (2 + $e0_cost + $e1_cost + $e2_cost),
                    [fold => $e0->[1], $e1->[1], [ 'lambda', '(y z)', $e2->[1]]]];
                }
              }
            }
          }

          # (tfold x 0 (lambda (y z) e2))
          when('tfold') {
            my $e0s = [[1, 'x']];
            foreach my $e0 (@$e0s) {
              my $e0_cost = $e0->[0];
              my $e1s = [[1, 0]];
              foreach my $e1 (@$e1s) {
                # my $iftrue = clone $iftrue;
                my $e1_cost = $e1->[0];
                $is_tfold = 1;
                my $e2s = gen_exp($max_cost - $e0_cost - $e1_cost - 2, $ops);
                $is_tfold = 0;
                foreach my $e2 (@$e2s) {
                  my $e2_cost = $e2->[0];
                  push @results, [
                    (2 + $e0_cost + $e1_cost + $e2_cost),
                    [tfold => $e0->[1], $e1->[1],[ 'lambda', '(x y)',  $e2->[1]]]];
                }
              }
            }
          }

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
  print "Generating solutions";
  my $combos = gen_exp($cost - 1, $ops_struct);
  print "\n";
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
  # return $_[0];
  # say "dump: " . Dumper([@_]);
  # my $f = Dumper([@_]);
  my $f = nfreeze([@_]);
  # my $f = "$_[0]";
  # my $f = render($_[0]);# . ",$_[1]";
  # say STDERR "norm: $f";
  return $f;
}
# memoize('gen_exp',
  # NORMALIZER => 'gen_exp_normalize',
# );

# memoize('interp',
  # NORMALIZER => 'gen_exp_normalize',
# );

# memoize('compile',
  # NORMALIZER => 'gen_exp_normalize',
# );

func gridify($progs) {
  # my @inputs = map { uint64_rand() } 1..256;
  my @inputs = map { uint64_rand() } 1..246;
  push @inputs, map { uint64($_) } -4..5;
  # print Dumper(\@inputs);
  # my @inputs = map { uint64($_) } -127..128;

  my $parser = BV::Parse->new;
  my $grid = {};
  my $c = 0;
  print "Building grid\n";
  foreach my $program (@$progs) {
    print "$c/".(scalar(@$progs))."\r" unless $c++ % 100;
    my $parsed_program = $parser->parse($program);
    my @results = map { $parser->evaluate($parsed_program, $_) } @inputs;
    @results = map { sprintf "0x%016X", $_ } @results;
    my $r = join(',',@results);
    $grid->{$r} //= [];
    push $grid->{$r}, $program;
  }
  print "\n";
  @inputs = map { sprintf "0x%016X", $_ } @inputs;
  return [\@inputs, $grid];
}

func treeify($progs) {
  print "Building tree";
  my @inputs = map { uint64_rand() } 1..246;
  push @inputs, map { uint64($_) } -4..5;
  my $parser = BV::Parse->new;
  my @progs = map { [ $parser->parse($_), $_ ] } @$progs;
  return [\@inputs, treeify_helper(\@progs, [@inputs])];
}

my $treec = 0;
func treeify_helper($progs, $inputs) {
  return $progs if ! @$inputs;
  # say "Inputs: @$inputs";
  my $parser = BV::Parse->new;
  my $input = shift @$inputs;
  my $tree = {};
  foreach my $program (@$progs) {
    my $parsed_program = $program->[0];
    my $result = $parser->evaluate($parsed_program, $input);
    print "." unless ++$treec % 1000;
    $result = sprintf "0x%016X", $result;
    $tree->{$result} //= [];
    push $tree->{$result}, $program;
  }

  # say "Treeify key count: " . scalar keys %$tree;
  # foreach my $k (sort keys %$tree) {
    # say "$k: " . scalar @{ $tree->{$k} };
  # }

  foreach my $output (keys %$tree) {
    if(scalar @{$tree->{$output}} > 50) {
      $tree->{$output} = treeify_helper($tree->{$output}, [ @$inputs ]);
    }
  }
  return $tree;
}

1;

