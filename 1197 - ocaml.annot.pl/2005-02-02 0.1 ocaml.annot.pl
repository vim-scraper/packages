#!/usr/bin/perl
# Given a OCaml file and an offset in that file, print out the type at this
# position, according to .annot

($f,$p) = ($ARGV[0],$ARGV[1]) ;
$f =~ s/\.ml$/\.annot/ || die "This is no ML!\n" ;

open A, $f ;
while(<A>){
  next unless /(?:\d+)\s(?:\d+)\s(\d+)\s.*?\s(?:\d+)\s(?:\d+)\s(\d+)/ ;
  next unless $1 <= $p && $p <= $2 ;

  $_ = <A> ; # assume it's "type ("
  chomp ($_ = <A>) ;
  s/^\s*// ;
  print ;
  exit 0 ;
}

close A ;
print " ??? " ;
