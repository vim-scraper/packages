#!/usr/bin/perl
# Given a OCaml file and an offset in that file, print out the type at this
# position, according to .annot

($f,$p) = ($ARGV[0],$ARGV[1]) ;
$f =~ s/\.ml$/\.annot/ || die "This is no ML!\n" ;

open A, $f ;
while(<A>){
  next unless /^"[^"]*"\s\d+\s\d+\s(\d+)\s"[^"]*"\s\d+\s\d+\s(\d+)$/sm;
  next unless $1 <= $p && $p <= $2;

  $_ = <A> ; # assume it's "type ("
  $result = "";
  while (<A>) {
    last if (/^\)$/);
    $result .= $_;
  }
  $result =~ s/^\s*//;
  $result =~ s/\s*$//g;
  $result =~ s/\s+/ /g;
  print $result;
  exit 0;
}

close A ;
print " ??? " ;
