#!/bin/env perl
#
# Author: Mikolaj Machowski ( mikmach AT wp DOT pl )
# Version: 2.0
# License: GPL v. 2
# Date: 25 Apr 2006
#
# Script for creation XML data file for Vim7 XML omni-completion from DTDs
# Requires: perlSGML (tested against 1997Sep18 version)

# USAGE:
#
#    dtd2vim <file.dtd> [<dialectname>]
#
# This command will create file <file.vim> (remove .dtd extension and add .vim;
# other extensions will remain intact).
#
# <dialectname> (not obligatory) will be part of dictionary name and will be
# used as argument for :XMLns command (file name - sans extensions) have to be
# the same.
#
# perlSGML and this script doesn't work with multiple files. User has to
# prepare single DTD file to parse all data.
#
# In created file global variable is named g:xmldata_<dialectname>. When second
# argument wasn't provided 'xxxx' will be used.
# After that place file in:
#
#    ~/.vim/autoload/xml
#
# directory. Of course it can be also global directory or other Vim data
# hierarchy of files. Example for  DocBook 4.4:
# DTD is in file docbook.dtd, call command with
#
#    dtd2vim.pl docbook.dtd docbook44
#
# Put file as:
#   
#    ~/.vim/autoload/xml/docbook44.vim
#
#  Omni-completion for DocBook 4.4 files will be started with:
#
#    :XMLns docbook44
#
#  command.
#
# Potential problems: not always properly detected vimxmlroot.
#
# ChangeLog:
# 1.1 (19 Apr)
#     - commented out generation of * to mark required attributes - too many
#     false positives
#     - skip more DTD keywords: NAME, NUMBER


# License:
#  Copyright (C) 2006 Mikolaj Machowski <mikmach@wp.pl>
#
#  This script is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Library General Public
#  License as published by the Free Software Foundation; either
#  version 2 of the License, or (at your option) any later version.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Library General Public License for more details.
#
#  You should have received a copy of the GNU Library General Public License
#  along with this library; see the file COPYING.LIB.  If not, write to
#  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
#  Boston, MA 02110-1301, USA.

use SGML::DTD;

if ( $#ARGV < 0 ){
	print "Script generating XML omni completion data file for Vim7\n";
	print "USAGE:\n";
	print "  dtd2vim.pl <filename> [<dialectname>]\n";
	exit 1;
}

open (FILE, "@ARGV[0]") or die "$!";

$dtd = new SGML::DTD;
$dtd->read_dtd(\*FILE);

close FILE;

@top_elements = $dtd->get_top_elements();
$tops = join "', '", @top_elements;
$tops = "'" . $tops . "'";


$" = ","; #" Dirty trick: highlighting cant cope with LIST_SEP. var
@entities_data = $dtd->get_gen_data_ents("0");

@entities = $dtd->get_gen_ents("0");
$ents = join "', '", @entities;
$ents = "'" . $ents . "'";

$output = @ARGV[0];
$output =~ s/\.dtd$//;
$output .= '.vim';
open (DATA, "> $output") or die "$!";

if (@ARGV > 1){
	$dialect = @ARGV[1];
} else {
	$dialect = 'xxxx';
}

print DATA "let g:xmldata_$dialect = {\n";
print DATA "\\ 'vimxmlentities': [$ents],\n";
print DATA "\\ 'vimxmlroot': [$tops],\n";

# Initialize tag info string
$taginfo = '';

@list_of_elements = $dtd->get_elements();
foreach $element (@list_of_elements) {

	# Get possible childs of current element
	@element_childs = $dtd->get_base_children($element);
	@element_childs = (@element_childs, $dtd->get_inc_children($element));
	@element_nochilds = $dtd->get_exc_children($element);
	# Remove pcdata if first value of table
	if ($element_childs[0] =~ /pcdata/) {
		shift @element_childs;
	}
	foreach $child (@element_childs) {
		$i = 0;
		foreach $nochild (@element_nochilds) {
			if ( $child eq $nochild ) {
				$i = $i + 1;
			}
		}
		if ( $i == 0 ) {
			push @children, $child;
		}
	}
	@element_childs = @children;
	undef @children;
	$childs = join "', '", @element_childs;
	$childs = "'" . $childs . "'";
	$childs =~ s/'EMPTY'//;

	if ( $childs =~ '^$') {
		$taginfo .= "\\ '$element': ['/>', ''],\n";
	}

	print DATA "\\ '".$element."': [\n\\ [$childs],\n";

	%element_attributes = $dtd->get_elem_attr($element);
	@attr_names = keys %element_attributes;
	# @attr_values = values %element_attributes;
	print DATA '\ { ';
	$attrs = '';
	foreach $attr_name (@attr_names) {
		foreach $item (@{$element_attributes{$attr_name}}) {
			if ($item !~ /(IMPLIED|FIXED|REQUIRED|CDATA|ID|ENTIT|NAME|NUMBER|NMTOKEN|NOTATION)/) {
				# IDREF, IDREFS, NMTOKENS, ENTITIES will be cared of by super-strings
				push @val, $item;
			}
			#if ($item =~ /REQUIRED/) {
				#push @req, $attr_name;
				#$req{$attr_name} = '1';
				#}
		}
		if (@val) {
			$attr_vls = join "', '", @val;
			$attr_vls = "'" . $attr_vls . "'";
			undef @val;
		}
		$attrs .= "'".$attr_name ."': [$attr_vls], ";
		undef $attr_vls;
	}
	$attrs =~ s/..$//;
	print DATA $attrs;
	print DATA "}\n\\ ],\n";
}

#$req = join "': ['*', ''],\n\\ '", keys %req;
#$req = "\\ '" . $req . "': ['*', '']";
#print DATA "\\ 'vimxmlattrinfo': {\n$req\n\\ },\n";
print DATA "\\ 'vimxmltaginfo': {\n$taginfo\\ }";

# Close big dictionary and add modeline to data file
print DATA "\n\\ }\n\" vim:ft=vim:ff=unix";

# Space necessary to get above line from default 'modelines' range
#
#
#

close DATA or die "$!";

# vim:isk+=$,%:ft=perl:ff=unix:tw=80:fo+=o:
