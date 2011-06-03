""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" substitute.vim -- mappings for the s/// command
" 
" Author: Anders Thøgersen
" Last Change: 27-Sep-2005
" Version:     1.04
"
" Licence: This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License. See
" http://www.gnu.org/copyleft/gpl.txt 
"
" Download From:
" http://www.vim.org/scripts/script.php?script_id=1167
" 
" Description:
" Visual and normal mode mappings for easy access to substitutions with the
" s/// command.
" 
" This script will make it easy to replace the word under the cursor, or the
" visually selected text across the whole file. If more than one line is
" selected the substitution will only occur on the selected lines.
" 
" History:
" 
" 1.04 - Added a separate helpfile and the possibility of customizing 
"        mappings and the register that the script uses.
" 
" 1.03 - Simplified the mapping a bit. Marking register ' was not needed.
" 
" 1.02 - Changed the meaning of the ;' mapping, and added <unique> to the map
"        definitions.  Added the Escape function so $ and ^ are only escaped when they
"        appear at the beginning, or at the end.  Also added cpoptions check and <SID>
"        stuff.
" 
" 1.01 - Removed unuseful mapcheck.
" 
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if exists('loaded_substitute') || &cp
  finish
endif
let loaded_substitute = 1

let s:savedCpo = &cpoptions
set cpoptions&vim

" Configuration
if !exists("g:substitute_PromptMap")
	let g:substitute_PromptMap = ";'"
endif
if !exists("g:substitute_NoPromptMap")
	let g:substitute_NoPromptMap = ';;'
endif
if !exists("g:substitute_Register")
	let g:substitute_Register = '9'
endif

" define the mappings 
exe 'nnoremap <unique> '. g:substitute_NoPromptMap .' yiw:let @'. g:substitute_Register ."=':%'.<SID>SubstituteAltSubst(@\", 'g')<Cr>@". g:substitute_Register
exe 'nnoremap <unique> '. g:substitute_PromptMap   .' yiw:let @'. g:substitute_Register ."=':%'.<SID>SubstituteAltSubst(@\", 'gc')<Cr>@". g:substitute_Register
exe 'vnoremap <unique> '. g:substitute_NoPromptMap .' <ESC>gvy:let @'. g:substitute_Register ."=<SID>SubstituteVisualAltSubst(@\", 'g')<Cr>@". g:substitute_Register
exe 'vnoremap <unique> '. g:substitute_PromptMap   .' <ESC>gvy:let @'. g:substitute_Register ."=<SID>SubstituteVisualAltSubst(@\", 'gc')<Cr>@". g:substitute_Register
cnoremap <unique> <C-R><C-R> <C-R>"

" Remove the default key sequences
unlet g:substitute_Register
unlet g:substitute_PromptMap
unlet g:substitute_NoPromptMap

fun! <SID>SubstituteAltSubst(txt, flags)
	let d = s:GetSubstDelimiter(a:txt)
	let mv = '€kl€kl'
	if a:flags == 'gc'
		let mv = mv . '€kl' 
	endif
	if strlen(a:txt)==0
		let mv = mv . '€kl'
	endif
	let @" = s:Escape(a:txt) 
	return 's' .d . @" .d .d . a:flags . mv 
endfun

fun! <SID>SubstituteVisualAltSubst(txt, flags)
	let mv = '€kl€kl€kl'
	if a:flags == 'gc'
		let mv = mv . '€kl' 
	endif
	if line("'<")!=line("'>") || (line("'<")==line("'>") && col("'<")==1 && col("'>")==col("$"))
		let d = s:GetSubstDelimiter(a:txt)
		return ":'<,'>s" .d .d .d . a:flags . mv
	else
		return ':%' . <SID>SubstituteAltSubst(a:txt, a:flags)	
	endif
endfun

" feel free to add more :-)
fun! <SID>GetSubstDelimiter(txt)
	if stridx(a:txt, '/') == -1
		return '/'
	elseif stridx(a:txt, ':') == -1
		return ':'
	elseif stridx(a:txt, '#') == -1
		return '#'
	elseif stridx(a:txt, ';') == -1
		return ';'
	elseif stridx(a:txt, '!') == -1
		return '!'
	else 
		return '*'
	endif
endfun

" escape as little as possible
fun! <SID>Escape(txt)
	let esc = '\\.~[]'
	if stridx(a:txt, '$') == (strlen(a:txt) -1)
		let esc = esc . '$'
	endif
	if stridx(a:txt, '^') == 0
		let esc = esc . '^'
	endif
	if stridx(a:txt, '*') > 0
		let esc = esc . '*'
	endif
	return escape(a:txt, esc)
endfun

let &cpoptions = s:savedCpo

" HelpExtractor:
"  Author:	Charles E. Campbell, Jr.
"  Version:	3
"  Date:	May 25, 2005
"
"  History:
"    v3 May 25, 2005 : requires placement of code in plugin directory
"                      cpo is standardized during extraction
"    v2 Nov 24, 2003 : On Linux/Unix, will make a document directory
"                      if it doesn't exist yet
"
" GetLatestVimScripts: 748 1 HelpExtractor.vim
" ---------------------------------------------------------------------
set lz
let s:HelpExtractor_keepcpo= &cpo
set cpo&vim
let docdir = expand("<sfile>:r").".txt"
if docdir =~ '\<plugin\>'
 let docdir = substitute(docdir,'\<plugin[/\\].*$','doc','')
else
 if has("win32")
  echoerr expand("<sfile>:t").' should first be placed in your vimfiles\plugin directory'
 else
  echoerr expand("<sfile>:t").' should first be placed in your .vim/plugin directory'
 endif
 finish
endif
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
let &cpo= s:HelpExtractor_keepcpo
unlet s:HelpExtractor_keepcpo
finish

" ---------------------------------------------------------------------
" Put the help after the HelpExtractorDoc label...
" HelpExtractorDoc:
*substitute.txt*    Mappings for performing substitution     September 27, 2005

Author: Anders Thøgersen <anderslt [at] gmail.com>
Version: 1.04

================================================================================
Contents

	1. Purpose..............: |substitute-purpose|
	2. Mappings.............: |substitute-maps|
	3. Configuration........: |substitute-config|

================================================================================
1. Purpose                                *substitute-purpose*

The substitute.vim script provides shortcuts for replacing
the text under the cursor, or visually selected text in the
current file.  It is essentially a shortcut to the vim built
in substitute command; see |:substitute|.

There are two mappings defined: one which will replace text
across the whole file without prompting, and one which will
prompt for every replacement.

It should be noted that this script modifies a register when
one of the mappings is executed.  This register defaults to
"9 but it can be changed; see |substitute-config|.

================================================================================
2. Key mappings                              *substitute-maps*

substitute.vim provides the following two mappings for both
visual and normal mode:

   ;;  Perform substitution without prompting.

   ;'  Perform substitution and prompt for each.

These mappings can be changed as described in the following
section.

It is also possible to press <C-R><C-R> while in command
line mode to insert the text that is replaced into the 
commandline.

================================================================================
3. Configuration                           *substitute-config*

To change the default mappings to mappings that you like
better you can change the value of the following variables
before sourcing the substitute.vim script:

                                           *substitute-prompt*
   let g:substitute_PromptMap = ";'"

This variable contains the key sequence for invoking
substitution that should prompt each time a match is
found.

                                         *substitute-noprompt*
   let g:substitute_NoPromptMap = ';;'

This variable specifies the key sequence for invoking
substitution without prompting.

                                         *substitute-register*
To change the register that is overwritten when executing
one of the provided mappings you can change the value of
this variable:

   let g:substitute_Register = '9'

================================================================================
