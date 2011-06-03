#!/bin/bash

##############################################################################
#
# Vim Script Updater 0.5.1
#
# Author: David Munger
#
##############################################################################
#
# Dependencies:
# - curl (URL downloader)
# - xsltproc (from libxslt)
# - tar
# - unzip
#
##############################################################################

VIM=vim
BASEURL="http://www.vim.org/scripts"


##############################################################################
# usage
##############################################################################

usage() {
	cat >&2 <<-EOF
	usage: `basename $0` watchlist

	where watchlist is a file containing one or more lines with the following
	syntax:
	    script ID
	or
	    script ID|prefix

	Example file:
	    42
	    1259|colors
	    3096|plugin
	    3108
EOF
}

##############################################################################
# temporary files
##############################################################################

cleanup() {
	test -n "$TMPDIR" && rm -rf $TMPDIR
}

trap cleanup EXIT

TMPDIR=`mktemp -d`


##############################################################################
# check watch list
##############################################################################

check_watch_list() {
	local watchlist="$1"; shift
	test -s "$watchlist" && return 0
	usage
	exit 1
}

##############################################################################
# convert directory to vba
##############################################################################

dir_to_vba() {
	local src="$1"; shift
	local dst="$1"; shift

	find "$src" -type f | sed -e "s;^$src/;;" > $TMPDIR/vbalist.txt
	$VIM -e -c "%MkVimball! $dst $src" -c 'qa!' $TMPDIR/vbalist.txt
}

##############################################################################
# unzip file
##############################################################################

unzip_to() {
	local dst="$1"; shift
	local src=`readlink -m "$1"`; shift

	mkdir -p "$dst"

	case `file --brief --mime-type "$src"` in
		application/zip)
			unzip -q "$src" -d "$dst"
			;;
		application/x-gzip)
			tar -C "$dst" xzf "$src"
			;;
		application/x-bzip2)
			tar -C "$dst" xjf "$src"
			;;
		*)
			echo "unknown archive type $src"
			exit 1
			;;
	esac
}


##############################################################################
# install vimball
##############################################################################

install_vba() {
	local vba="$1"; shift
	echo "installing vimball $vba..."
	$VIM -e "$vba" -c "so %" -c "qa!" || ( echo "couldn't install vimball" ; return 1 )
}

##############################################################################
# download script
##############################################################################

download_script() {
	local name="$1"; shift
	local url="$1"; shift
	local dst="$1"; shift

	# confirm download
	ans=x
	while [[ "$ans" != "y" && "$ans" != "n" ]]
	do
		read -n1 -p "download $name? " ans
	done
	echo
	test "$ans" == "n" && return 1

	# download
	#echo "$url --> $dst"
	curl --progress-bar -o "$dst" "$url"
	return 0
}

##############################################################################
# update script
##############################################################################

update_script() {

	local name="$1"; shift
	local prefix="$1"; shift
	local file="$1"; shift
	local version="$1"; shift
	local url="$BASEURL/$1"; shift
	
	# check file extension
	case $file in
		*.vim.gz|*.vba.gz)
			;;
		*.vim.bz2|*.vba.bz2)
			;;
		*.vim|*.vba|*.zip|*.tgz|*.tar.gz|*.tar.bz2)
			;;
		*)
			echo "unknown file type for $file; aborting"
			return
			;;
	esac

	# download
	download_script "$name" "$url" "$TMPDIR/$file" || return 1
	test -s "$TMPDIR/$file" || ( echo "something went wrong" ; return 1 )

	# unzip if needed
	case $file in
		*.vim.gz|*.vba.gz)
			gunzip "$TMPDIR/$file"
			file=${file%%.gz}
			;;
		*.vim.bz2|*.vba.bz2)
			gunzip "$TMPDIR/$file"
			file=${file%%.bz2}
			;;
	esac

	# install
	case $file in

		*.vim)
			test -z "$prefix" && ( echo "a prefix must be specified for a .vim file" ; return 1 )

			mkdir -p "$TMPDIR/$name.dir/$prefix"
			mv "$TMPDIR/$file" "$TMPDIR/$name.dir/$prefix/"
			dir_to_vba "$TMPDIR/$name.dir" "$TMPDIR/$name.vba"
			install_vba "$TMPDIR/$name.vba" || return 1
			;;
		*.vba)
			install_vba "$TMPDIR/$file" || return 1
			;;
		*)
			# vimballize
			unzip_to "$TMPDIR/$name.dir/$prefix" "$TMPDIR/$file"
			dir_to_vba "$TMPDIR/$name.dir" "$TMPDIR/$name.vba"
			install_vba "$TMPDIR/$name.vba" || return 1
			;;
	esac

	return 0
}

##############################################################################
# update watch list
##############################################################################

get_script_info() {

	local sid="$1"; shift

	local tmpxsl=$TMPDIR/html2desc.xsl

	cat > $tmpxsl <<-EOF
	<?xml version="1.0" encoding="utf-8"?>
	<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
		<xsl:template match="/">
			<xsl:apply-templates select="/html/body/table[2]//table//table[3]/tr[2]"/>
		</xsl:template>
		<xsl:template match="/html/body/table//table//table/tr">
	<xsl:value-of select="td[1]"/>|<xsl:value-of select="td[2]"/>|<xsl:value-of select="td[3]"/>|<xsl:value-of select="td/a/@href"/>
		</xsl:template>
	</xsl:stylesheet>
EOF

	echo "updating information on script $sid..." >&2
	curl --progress-bar "$BASEURL/script.php?script_id=$sid" |
	sed 's/&/&amp;/g' |
	xsltproc --html $tmpxsl - |
	tail -n1 |
	sed -e 's/^/'$sid'|/'
}
   

##############################################################################
# MAIN
##############################################################################

WATCHLIST="$1"

if [[ -z "$WATCHLIST" ]]
then
	usage
	exit 1
fi

check_watch_list "$WATCHLIST"

for entry in `cat "$WATCHLIST"`
do
	# skip a line for clarity
	echo
	
	# parse old entry
	eval `echo "$entry" | sed -e 's#^\s*\([0-9]*\)\(|\([^|]*\)\(|\([^|]*\)|\([^|]*\)|\)\?\)\?.*#\
		sid=\1;prefix=\3;file1=\5;version1=\6#'`

	test -z "$sid" && echo "cannot parse entry: $entry" && continue

	# parse new entry
	eval `get_script_info $sid | sed -e 's#^'$sid'|\([^|]*\)|\([^|]*\)|\([^|]*\)|\([^|]*\)#\
		file2=\1;version2=\2;date2=\3;url2=\4#; t; d'`

	# guess name
	name=`echo "$file2" | sed -e 's/\([^\.]*\).*/\1/'`

	# check for name
	test -z "$name" && echo "cannot parse updated script info for ID $sid; skipping" && continue

	# check for version
	test -z "$version1" && version1="[?]"
	test -z "$version2" && echo "cannot parse script updated info for $name; skipping" && continue

	if [[ "$version1" != "$version2" ]]
	then
		# update script
		echo "$name updated on $date2 from $version1 to $version2"
		update_script "$name" "$prefix" "$file2" "$version2" "$url2" || continue
		
		# update watch list entry
		sed -i -e 's;^\s*'$sid'\>.*;'$sid'|'$prefix'|'$file2'|'$version2'|'$date2'|'$url2';' "$WATCHLIST"
	else
		echo "$name ($sid) already up-to-date"
	fi
done
