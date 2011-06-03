#!/usr/bin/perl -w
# colour_flip.pl version 1.0
# Copyright (c) 2002, Matthew Hawkins
# Released under the terms of version 2 of the GNU General Public License
# (GPL) as published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful, but it
# comes WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GPL for
# more details.
#
# This script was roughly 2 hours work, most of which was me beating
# my head against the desk repeatedly as Perl refused to do what I told it.

use strict;

my $newval;
my @stuff;
my $i;
my $rgbfile = "/usr/X11R6/lib/X11/rgb.txt";
my %rgb;
my $cval;
my $cname;

# Make a hash table from the entries in the rgb.txt file
open RGB,"<$rgbfile";
while (<RGB>) {
	if (m/(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})\s+(.*)/) {
		$cval = sprintf "%02x%02x%02x",$1,$2,$3;
		$cname = $4;
		$rgb{$cname} = $cval;
	}
}
close RGB;
# Add to the hash table some common colours
$rgb{'Blue'} = "#0000FF";
$rgb{'Red'} = "#FF0000";
$rgb{'Green'} = "#00FF00";
$rgb{'Yellow'} = "#FFFF00";
$rgb{'Cyan'} = "#00FFFF";
$rgb{'Magena'} = "#FF00FF";
$rgb{'Black'} = "#000000";
$rgb{'White'} = "#FFFFFF";
$rgb{'Gray'} = "#808080";
$rgb{'Grey'} = $rgb{'Gray'};
$rgb{'Brown'} = $rgb{'Yellow'};

# Debugging output of the %rgb hash
#my $key;
#foreach $key (keys %rgb) {
#        print "$rgb{$key} $key\n";
#}

# Read input from wherelse but standard input
while (<>) {
	# We're not interested in messing with non-highlight lines
	if (!/^hi/) {
		print;
		next;
	}
	# highlight lines consist of 4 things.  The highlight command,
	# the syntax object its operating on, the guifg/guibg/ctermfg/ctermbg
	# options we want to mess with, and any other misc crap ;)
	# We straight away can output the first two...
	@stuff = split;
	printf "%s %-14s ", $stuff[0],$stuff[1];
	# ...but since the rest can come in any order, we must mess with them
	for $i ( 2..$#stuff ) {
		&messwith($stuff[$i]);
	}
        print "\n";
}
exit 0;

# Colours come in two forms - RGB values or system colour names.
# Figure out what we have in this item, and deal with it.
sub messwith
{
	my $item;
	my $value;
	my $string = shift;
	($item, $value) = split('=', $string);
	if ($item =~ m/(?:gui|cterm)(?:f|b)g/) {
		print " $item=";
		if ($value =~ m/^#[[:alnum:]]{6}/) {
			printf "#%06x", flipvalue($value);
		} else {
			print &convertfromRGBfile($value);
		}
	} else {
                printf " %s=%s",$item,$value;
	}
}

# if the system colour name exists in our hash table, substitute it for
# its appropriate value, then flip that.  If it doesn't exist, just
# let it be.
# FIXME - that's a minor nit, btw ;)  Not much I can do about it though.
sub convertfromRGBfile
{
	my $incolour = shift;
	my $outcolour;
	$outcolour = $rgb{$incolour};
	if (defined $outcolour) {
		$outcolour = sprintf("#%06x",flipvalue($outcolour));
	} else {
		$outcolour = $incolour;
	}
	return $outcolour;
}

# We inverse RGB values by subtracting them from 0xffffff
# Just make sure we get rid of any leading hash mark first
sub flipvalue
{
	my $incolour = shift;
	if ($incolour =~ m/#([[:alnum:]]{6})/) {
		$incolour = $1;
	}
	my $outcolour = 0xffffff-hex($incolour);
	return $outcolour;
}
