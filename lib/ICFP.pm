package ICFP;

use local::lib 'local';
use everywhere 'v5.14; use Moo; use Method::Signatures::Simple',
  matching => '^(ICFP|BV)((?!Role).)*$',
  package_level => 1;

1;

