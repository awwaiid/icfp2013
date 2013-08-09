package BV::Parse;

has tree => ( is => 'rw' );

has vars => ( is => 'rw' );

# Now THIS is what I call a parser. Suck it.
method parse($str) {
  $self->vars({}); # reset vars
  $str =~ tr/()/[]/;
  $str =~ s/ /,/g;
  $str =~ s/(\w+)/'$1'/g;
  return eval($str);
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
      ...;
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
    when(/^[a-z0-9_]+/) { $self->vars->{$op} }

    default { die "UNKNOWN OP/VAL $op" }
  }
}

1;

