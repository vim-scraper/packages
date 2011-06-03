# $Id: zshrc-local-pauli,v 1.12 2002/03/15 21:05:50 8host Exp $

#
# Author: Lubomir Host 'rajo' <host8@kepler.fmph.uniba.sk>
#

# Source this with zsh (~/.zshrc) and type 'update-vim-zsh'

# Please call only function 'update-vim-zsh'


# Script automaticaly download new patches for vim, patch sources,
# and then compile & install vim

################################################################
# User's setup
################################################################

# Define vim version:
VIM_VER="vim61b"
VIM_VER_LONG="vim-6.1b"
VIM_VER_EGREP="^6\.1b\."

# Web site where patches are located
#URL="ftp://ftp.vim.org/pub/vim/patches/"
URL="ftp://ftp.vim.org/pub/vim/unstable/patches/"

# Directory where Vim sources are located
#SRC_DIR="/opt/scratch/rajo/src/own"
SRC_DIR="/opt/scratch/rajo/src/own/unstable"

# Where save tar-bziped archiv
DIST_DIR="$HOME/public_html/vim/dist"

# '6.0.0.099' --> '98'
# for version 6.0.001 return empty string
old_version()
{
#	if [ "x$1" = "x6.0.001" ]; then
#		echo;
#		return;
#	fi
#	echo $1 | awk 'BEGIN {FS="."} { print "." $3 - 1 }'
#	echo $1 | awk 'BEGIN {FS="."} { print ".0" $3 - 1 }'
	#echo $1 | awk ' BEGIN {FS="."} { if ($3 < 11) { print ".00" $3 - 1 ; next; } if ($3 < 101) { print ".0" $3 - 1 ; next; } print "." $3 - 1 ; }'
# For instipration thanks to 'Nebojsa'
	echo $1 | awk 'BEGIN {FS="."} { printf ".%03d\n", $3 - 1 }'
}

# '6.0.099' --> '99'
new_version()
{
#	echo $1 | awk 'BEGIN {FS="."} { print "." $3 - 1 + 1 }'
	echo $1 | awk 'BEGIN {FS="."} { print "." $3  }'
}

download_patch()
{
	local DIR
	DIR=`pwd`;
	cd $SRC_DIR/patches && \
		wget -nH -nd $URL/$1 || \
			( sleep 100;
				wget -nH -nd $URL/$1 ) || return 1 ;
	cd $DIR
}

patch_sources()
{
	local DIR
	DIR=`pwd`;
	cd $SRC_DIR/patches && \
		patch -p0 < $1 && \
			cd $SRC_DIR && \
				rm -f $VIM_VER
				mv $VIM_VER`old_version $1` $VIM_VER`new_version $1` && \
				ln -s $VIM_VER`new_version $1` $VIM_VER || return 1
	cd $DIR
}

make_distribution()
{
	local DIR DIST_FILE OLD_DIST_FILE
	DIR=`pwd`;
	DIST_FILE="$DIST_DIR/$VIM_VER_LONG`new_version $1`-all.tar.bz2"
	OLD_DIST_FILE="$DIST_DIR/$VIM_VER_LONG`old_version $1`-all.tar.bz2"
	cd /opt/scratch/rajo/src/vim/ && \
		tar cIf $DIST_FILE $VIM_VER`new_version $1` && \
			chmod a+r $DIST_FILE && \
				rm -f $OLD_DIST_FILE || return 1
	cd $DIR
}

update-vim-zsh()
{
	local DIR TMP_FILE
	DIR=`pwd`;
	TMP_FILE=`tempfile -p update_vim-zsh || mktemp /tmp/update_vim-zshXXXXXX || echo /tmp/update_vim-zsh$$`
	echo "Looking for new patches..."; 
	wget -O - $URL 2> /dev/null | awk 'BEGIN { FS=">" } {print $2}' | \
		egrep "$VIM_VER_EGREP" | sed 's/<.*//g;' > $TMP_FILE
	
	cd $SRC_DIR/patches && \
	(
		NEW_PATCHES="`ls -1| egrep "$VIM_VER_EGREP" | diff - $TMP_FILE | awk '{ print $2 }'`"
		if [ -z "$NEW_PATCHES" ]; then
			echo "No new patches..."
		else
			for i in `echo $NEW_PATCHES`
			do
				download_patch $i && patch_sources  $i || exit 1
			done
			cd $SRC_DIR/$VIM_VER && make && make install
		fi
		
	)
	rm -f $TMP_FILE
	cd $DIR
}

