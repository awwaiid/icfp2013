#!/usr/bin/env perl

use v5.16;
use Parallel::ForkManager;
use File::Slurp;
use IO::Handle;

STDOUT->autoflush(1);
STDERR->autoflush(1);
STDIN->autoflush(1);

say "Syncing local -> CSU";
say `cd .. ; rsync -avzP assignment3 endive.cs.colostate.edu:`;

my $timeout = 300;

my @machines = read_file('avail_machines.txt', { chomp => 1 });
@machines = (@machines) x 2; # 4 per machine

my $cur_machine = 0;
# my %machines = map { ( $_ => 0 ) } @machines;

# sub avail_machine {
  # my @avail = grep { $machines{$_} == 0 } keys %machines;
  # die "ERROR: NO MACHINE AVAILABLE!" unless @avail;
  # return $avail[0];
# }

my @problems = qw(
  atspSet.atsp
);

  # 192Cities.atsp
  # 100cities.atsp
  # city200.atsp
  # santa600.atsp
  # smallDist.tsp
  # atex5-1.atsp


  # canada-1.atsp
  # chemreact.atsp
  # coral.atsp
  # dc563.atsp
  # Freed_Assignment2.atsp
  # galaxy40.tsp
  # ha30_dist.atsp
  # it101.atsp
  # lotr123.tsp
  # martin.atsp
  # MJM.atsp
  # nCube.atsp
  # nfl32.tsp
  # nodes600.atsp
  # santa600.atsp
  # sgb128_dist.atsp
  # testData.tsp
  # usca312.tsp
  # xpr2308.tsp
  # xql662_converted.tsp

# my @problems = split(/\n/, `ls problems`);

say "Estimated time: " . ($timeout * @problems * 3 / @machines / 60) . " min";

my $pm = Parallel::ForkManager->new(scalar @machines);

foreach my $iter (1..3) {

  foreach my $problem (@problems) {
    my $machine = $machines[$cur_machine++];
    $cur_machine %= scalar @machines;
    if(! $pm->start) {
      # say "$machine\tasn1\t$problem\t$iter\tstart";
      # say "ssh $machine.cs.colostate.edu 'cd assignment3 ; nice algo/asn1/atsp problems/$problem out_result/asn1_$problem.$iter.result $timeout > out_msg/asn1_$problem.$iter.out'";
      # print `ssh $machine.cs.colostate.edu 'cd assignment3 ; nice algo/asn1/atsp problems/$problem out_result/asn1_$problem.$iter.result $timeout > out_msg/asn1_$problem.$iter.out'`;
      # say "$machine\tasn1\t$problem\t$iter\tdone";
      $pm->finish;
    } else {
      $machine = $machines[$cur_machine++];
      $cur_machine %= scalar @machines;
      $pm->start and next;
      say "$machine\tasn2\t$problem\t$iter\tstart";
      say "ssh $machine.cs.colostate.edu 'cd assignment3 ; nice algo/asn2/atsp -t $timeout problems/$problem out_result/asn2_$problem.$iter.result > out_msg/asn2_$problem.$iter.out'";
      print `ssh $machine.cs.colostate.edu 'cd assignment3 ; nice algo/asn2/atsp -t $timeout problems/$problem out_result/asn2_$problem.$iter.result > out_msg/asn2_$problem.$iter.out'`;
      say "$machine\tasn2\t$problem\t$iter\tdone";
      $pm->finish;
    }

  }

}

$pm->wait_all_children;

say "Syncing CSU -> local";
say `cd .. ; rsync -avzP endive.cs.colostate.edu:assignment3 ./`;

