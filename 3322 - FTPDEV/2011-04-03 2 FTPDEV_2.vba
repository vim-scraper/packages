" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/vim_ftpdev.vim	[[[1
162
" Title:  Vim filetype plugin file
" Author: Marcin Szamotulski
" Email:  mszamot [AT] gmail [DOT] com
" Last Change:
" GetLatestVimScript: 3322 2 :AutoInstall: FTPDEV
" Copyright Statement: 
" 	  This file is a part of Automatic Tex Plugin for Vim.
"
"     Automatic Tex Plugin for Vim is free software: you can redistribute it
"     and/or modify it under the terms of the GNU General Public License as
"     published by the Free Software Foundation, either version 3 of the
"     License, or (at your option) any later version.
" 
"     Automatic Tex Plugin for Vim is distributed in the hope that it will be
"     useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
"     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
"     General Public License for more details.
" 
"     You should have received a copy of the GNU General Public License along
"     with Automatic Tex Plugin for Vim.  If not, see <http://www.gnu.org/licenses/>.
"
"     This licence applies to all files shipped with Automatic Tex Plugin.
if !exists("g:ftplugin_dir")
    let g:ftplugin_dir	= globpath(split(&rtp, ',')[0], 'ftplugin') . ',' . globpath(split(&rtp, ',')[0], 'plugin')
endif
try
function! Goto(what,bang,...)
    let grep_flag = ( a:bang == "!" ? 'j' : '' )
    if a:what == 'function'
	let pattern		= '^\s*fu\%[nction]!\=\s\+\%(s:\|<SID>\)\=' .  ( a:0 >=  1 ? a:1 : '' )
    elseif a:what == 'command'
	let pattern		= '^\s*com\%[mand]!\=\s\+.*\s*' .  ( a:0 >=  1 ? a:1 : '' )
    elseif a:what == 'variable'
	let pattern 		= '^\s*let\s\+' . ( a:0 >=  1 ? a:1 : '' )
    elseif a:what == 'map'
	let pattern		= '^\s*[cilnosvx!]\=\%(nore\)\=m\%[ap]\>.*' . ( a:0 >= 1 ? a:1 : '' )
    else
	let pattern 		= '^\s*[ci]\=\%(\%(nore\|un\)a\%[bbrev]\|ab\%[breviate]\)' . ( a:0 >= 1 ? a:1 : '' )
    endif
	let g:pattern	= pattern
    let filename	= join(map(split(globpath(g:ftplugin_dir, '**/*vim'), "\n"), "fnameescape(v:val)"))
	let g:filename 	= filename

    let error = 0
    try
	exe 'vimgrep /'.pattern.'/' . grep_flag . ' ' . filename
    catch /E480:/
	echoerr 'E480: No match: ' . pattern
	let error = 1
    endtry
    if !error
	exe 'silent! normal zO'
	exe 'normal zt'
    endif
endfunction
catch /E127/
endtry
" Completion is not working for a very simple reason: we are edditing a vim
" script which might not be sourced.
command! -buffer -bang -nargs=? -complete=function Function 	:call Goto('function', <q-bang>, <q-args>) 
command! -buffer -bang -nargs=? -complete=command Command 	:call Goto('command', <q-bang>, <q-args>) 
command! -buffer -bang -nargs=? -complete=var Variable 		:call Goto('variable', <q-bang>, <q-args>) 
command! -buffer -bang -nargs=? -complete=mapping Map 		:call Goto('map', <q-bang>, <q-args>) 

" Search in current function
function! SearchInFunction(pattern, flag) 

    let [ cline, ccol ] = [ line("."), col(".") ]
    if a:flag =~# 'b\|w' || &wrapscan
	let begin = searchpairpos('^\s*fun\%[ction]\>', '', '^\s*endfun\%[ction]\>', 'bWn')
    endif
    if a:flag !~# 'b' || a:flag =~# 'w' || &wrapscan
	let end = searchpairpos('^\s*fun\%[ction]\>', '', '^\s*endfun\%[ction]\>', 'Wn')
    endif
    if a:flag !~# 'b'
	let pos = searchpos('\(' . a:pattern . '\|^\s*endfun\%[ction]\>\)', 'W')
    else
	let pos = searchpos('\(' . a:pattern . '\|^\s*fun\%[ction]\>\)', 'Wb')
    endif

    let msg="" 
    if a:flag =~# 'w' || &wrapscan
	if a:flag !~# 'b' && pos == end
	    let msg="search hit BOTTOM, continuing at TOP"
	    call cursor(begin)
	    call search('^\s*fun\%[ction]\zs', '')
	    let pos = searchpos('\(' . a:pattern . '\|^\s*endfun\%[ction]\>\)', 'W')
	elseif a:flag =~# 'b' && pos == begin 
	    let msg="search hit TOP, continuing at BOTTOM"
	    call cursor(end)
	    let pos = searchpos('\(' . a:pattern . '\|^\s*fun\%[ction]\>\)', 'Wb')
	endif
	if pos == end || pos == begin
	    let msg="Pattern: " . a:pattern . " not found." 
	    call cursor(cline, ccol)
	endif
    else
	if pos == end || pos == begin
	    let msg="Pattern: " . a:pattern . " not found." 
    	call cursor(cline, ccol)
	endif
    endif

    if msg != ""
	    echohl WarningMsg
	redraw
	exe "echomsg '".msg."'"
	    echohl Normal
    endif
endfunction
function! s:GetSearchArgs(Arg,flags)
    if a:Arg =~ '^\/'
	let pattern 	= matchstr(a:Arg, '^\/\zs.*\ze\/')
	let flag	= matchstr(a:Arg, '\/.*\/\s*\zs['.a:flags.']*\ze\s*$')
    elseif a:Arg =~ '^\i' && a:Arg !~ '^\w'
	let pattern 	= matchstr(a:Arg, '^\(\i\)\zs.*\ze\1')
	let flag	= matchstr(a:Arg, '\(\i\).*\1\s*\zs['.a:flags.']*\ze\s*$')
    else
	let pattern	= matchstr(a:Arg, '^\zs\S*\ze')
	let flag	= matchstr(a:Arg, '^\S*\s*\zs['.a:flags.']*\ze\s*$')
    endif
    return [ pattern, flag ]
endfunction
function! Search(Arg)

    let [ pattern, flag ] = s:GetSearchArgs(a:Arg, 'bcenpswW')
    let @/ = pattern
    call histadd("search", pattern)

    if pattern == ""
	echohl ErrorMsg
	redraw
	echomsg "Enclose the pattern with /.../"
	echohl Normal
	return
    endif

    call SearchInFunction(pattern, flag)
endfunction
command! -buffer -nargs=*	S 	:call Search(<q-args>) | let v:searchforward = ( s:GetSearchArgs(<q-args>, 'bcenpswW')[1] =~# 'b' ? 0 : 1 )
" my vim doesn't distinguish <C-n> and <C-N>:
nmap <silent> <buffer> <C-N>				:call SearchInFunction(@/,'')<CR>
nmap <silent> <buffer> <C-P> 				:call SearchInFunction(@/,'b')<CR>
nmap <silent> <buffer> gn 				:call SearchInFunction(@/,( v:searchforward ? '' : 'b'))<CR>
nmap <silent> <buffer> gN				:call SearchInFunction(@/,(!v:searchforward ? '' : 'b'))<CR>
function! PluginDir(...)
    if a:0 == 0 
	echo g:ftplugin_dir
    else
	let g:ftplugin_dir=a:1
    endif
endfunction
command! -nargs=? -complete=file PluginDir	:call PluginDir(<f-args>)

try
function! Pgrep(vimgrep_arg)
    let filename	= join(map(split(globpath(g:ftplugin_dir, '**/*vim'), "\n"), "fnameescape(v:val)"))
    execute "vimgrep " . a:vimgrep_arg . " " . filename 
endfunction
catch /E127:/
endtry
command! -nargs=1 Pgrep		:call Pgrep(<q-args>)
doc/ftpdev.txt	[[[1
114
*ftpdev.txt* 			For Vim version 7	Last change: 13 November 2010

		    A help file for <F>ile <T>ype <P>lugin <DEV>elopemnt  (ver 1)
				by Marcin Szamotulski
			    mszamot [AT] gmail [DOT] com
			----------------------------------------

This file type plugin provides some additional functions and commands which
help writing and vim plugins. Especialy big ones. Here is the list of
available commands and maps with some explanation:

NEWS							*ftpdev-news*	

In the version 2 of thus plugin you get:
    map gn
    man gN
	    maps to search in the current function for the last search pattern @/.
	    They work like |n| and |N|.

    The messages for this search are now visibe.

    GetLatestVimScript support added.

							|ftpdev-g:ftplugin_dir|
There is one variable which needs to be configured >
 g:ftplugin_dir
< This is the directory which will be searched for files. If you set this to
your ~/.vim/ftplugin directory where you have lots of scripts this plugin
might be a bit slow. Each plugin that I develop has its own directory (you
probably use some version control system as I do) then this vairable is set
in my vimrc file via an autocommand: >
	au BufEnter /some_directory/*.vim	let g:ftplugin=<value>
<

:Function[!] {fname}					*ftpdev-:Function*
	This finds function with name mathcing {fname} vim pattern under the
	|ftpdev-g:ftplugin_dir| directory. It uses the internal grep, i.e.
	|vimgrep|. {fname} should be as the first argument of |vimgrep| except
	it doesn't have to be put between /.../ .

	{fname} can not begin with <SID> or 's:' even if the function name has
	it.

	The bang vimgrep is supplied with the j switch, i.e. only quickfix
	list will be filled with mathes.
:Command[!] {cname}					*ftpdev-:Command*
	This finds command which name matches {fname} vim pattern under the
	|ftpdev-g:ftplugin_dir| {cname> has the same syntax as {fname}. The
	bang works as in |ftpdev-:Function|
:Variable[!] {vname}					*ftpdev-:Variable*
	This finds variable definition mathing {vname}. {vname} has to contain
	the g: b: s: t: prefix if the variable has it. The bang works as in
	|ftpdev-:Function|

:Map[!] {maplhs}					*ftpdev-:Map*
	This finds maps which lhs matches {maplhs} vim pattern. The bang works as in
	|ftpdev-:Function|

:S /{pattern}/
map gn
map gN
	This function make a search of {pattern} in the current function (it
	can wrap around the end of function. The argument {pattern} is a vim
	pattern passed to |vimgrep|.

	The pattern is added to search history and copied to the @/ register,
	thus you can re use it with |n| and |N| commands to search globaly or
	gn and gN to search in the scope of current function.

:PluginDir {directory}	
	This sets the variable |ftpdev-g:ftplugin_dir|. It has dir type
	completion.

:Pgrep /{pattern}/[j][g]
	This makes |vimgrep| in all files '*.vim' under |ftpdev-g:ftplugin_dir|.
	The argument syntax is the same as for |vimgrep|.
	

Remember: Vim has its own vim.vim ftplugin which defines: 
	]] 	(next begining of a function), 
	][ 	(next end of a function), 
	[[ 	(previous begining of a function), 
	[] 	(previous end of a function).
	

Happy viming :)	

================================================================================
COPY RIGHTS							*ftpdev-copy-rights*


    Copyright (C) 2010 Marcin Szamotulski Permission is hereby granted to use
    and distribute this code, with or without modifications, provided that
    this copyright notice is copied with it. 

    FTPDEV filetype plugin for Vim is free software: you can redistribute it
    and/or modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.
 
    FTPDEV filetype plugin for Vim is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
    Public License for more details.
 
    You should have received a copy of the GNU General Public License along
    with FTPDEV filetype plugin for Vim.  If not, see <http://www.gnu.org/licenses/>.

    This licence applies to all files shipped with FTPDEV filetype plugin.




" vim:tw=75:ts=8:ft=help:norl:
