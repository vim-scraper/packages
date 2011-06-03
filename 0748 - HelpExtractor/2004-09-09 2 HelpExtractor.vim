" HelpExtractor:
"  Author:	Charles E. Campbell, Jr.
"  Version:	3
"  Date:	Sep 09, 2004
"
"  History:
"    v2 Nov 24, 2003 : On Linux/Unix, will make a document directory
"                      if it doesn't exist yet
"
" GetLatestVimScripts: 748 1 HelpExtractor.vim
" ---------------------------------------------------------------------
set lz
let docdir = substitute(expand("<sfile>:r").".txt",'\<plugin[/\\].*$','doc','')
if !isdirectory(docdir)
 if has("win32")
  echoerr 'Please make '.docdir.' directory first'
  unlet docdir
  finish
 elseif !has("mac")
  exe "!mkdir ".docdir
 endif
endif

let curfile = expand("<sfile>:t:r")
let docfile = substitute(expand("<sfile>:r").".txt",'\<plugin\>','doc','')
exe "silent! 1new ".docfile
silent! %d
exe "silent! 0r ".expand("<sfile>:p")
silent! 1,/^" HelpExtractorDoc:$/d
exe 'silent! %s/%FILE%/'.curfile.'/ge'
exe 'silent! %s/%DATE%/'.strftime("%b %d, %Y").'/ge'
norm! Gdd
silent! wq!
exe "helptags ".substitute(docfile,'^\(.*doc.\).*$','\1','e')

exe "silent! 1new ".expand("<sfile>:p")
1
silent! /^" HelpExtractor:$/,$g/.*/d
silent! wq!

set nolz
unlet docdir
unlet curfile
"unlet docfile
finish

" ---------------------------------------------------------------------
" Put the help after the HelpExtractorDoc label...
" HelpExtractorDoc:
*%FILE%.txt*		The Help Extractor			*%DATE%*
Author: Charles E. Campbell, Jr.
Date:	Aug 26, 2003

==============================================================================
1. Contents					*%FILE%* *%helpextractor-contents*
	1. Contents ..................... : |helpextractor-contents|
    2. HelpExtractor ................ : |helpextractor|

==============================================================================
2. HelpExtractor						*helpextractor*

	The help extractor is intended to help plugin writers.  Instead of
	creating tar files, etc, simply modify your plugin.  Put the
	text of this file from

		" HelpExtractor:
		" HelpExtractorDoc:

	at the end of your plugin.  Then simply put your help file after the
	"Help ExtractorDoc:" label.  When the user puts your plugin in the
	.vim/plugin directory, the plugin will delete the HelpExtractor
	from it and create the help file (using the plugin's name, less
	the suffix, plus ".txt") in the ../doc directory.

vim:tw=78:ts=8:ft=help
