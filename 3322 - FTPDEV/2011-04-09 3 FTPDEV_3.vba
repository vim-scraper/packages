" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/vim_ftpdev.vim	[[[1
247
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
	let pattern		= '^\s*fu\%[nction]!\=\s\+\%(s:\|<\csid>\)\=' .  ( a:0 >=  1 ? a:1 : '' )
    elseif a:what == 'command'
	let pattern		= '^\s*com\%[mand]!\=\(\s*-buffer\s*\|\s*-nargs=[01*?+]\s*\|\s*-complete=\S\+\s*\|\s*-bang\s*\|\s*-range=\=[\d%]*\s*\|\s*-count=\d\+\s*\|\s*-bar\s*\|\s*-register\s*\)*\s*'.( a:0 >= 1 ? a:1 : '' )
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
	exe 'silent! vimgrep /'.pattern.'/' . grep_flag . ' ' . filename
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
command! -buffer -bang -nargs=? -complete=custom,FuncCompl Function 	:call Goto('function', <q-bang>, <q-args>) 
function! FuncCompl(A,B,C)
    let saved_loclist=getloclist(0)
    let filename	= join(map(split(globpath(g:ftplugin_dir, '**/*vim'), "\n"), "fnameescape(v:val)"))
    exe 'lvimgrep /^\s*fun\%[ction]/gj '.filename
    let loclist = getloclist(0)
    call setloclist(0, saved_loclist)
    call map(loclist, 'get(v:val, "text", "")')  
    call map(loclist, 'matchstr(v:val, ''^\s*fun\%[ction]!\=\s*\(<\csid>\|\cs:\)\=\zs.*\ze\s*('')')
    call map(loclist, 'v:val.''\>''')
    return join(loclist, "\n")
endfunction
function! CommandCompl(A,B,C)
    let saved_loclist=getloclist(0)
    let filename	= join(map(split(globpath(g:ftplugin_dir, '**/*vim'), "\n"), "fnameescape(v:val)"))
    exe 'lvimgrep /^\s*com\%[mand]/gj '.filename
    let loclist = getloclist(0)
    call setloclist(0, saved_loclist)
    call map(loclist, 'get(v:val, "text", "")')  
    call map(loclist, 'matchstr(v:val, ''^\s*com\%[mand]!\=\(\s*-buffer\s*\|\s*-nargs=[01*?+]\s*\|\s*-complete=\S\+\s*\|\s*-bang\s*\|\s*-range=\=[\d%]*\s*\|\s*-count=\d\+\s*\|\s*-bar\s*\|\s*-register\s*\)*\s*\zs\w*\>\ze'')')
    call map(loclist, 'v:val.''\>''')
    return join(loclist, "\n")
endfunction
command! -buffer -bang -nargs=? -complete=custom,CommandCompl Command 	:call Goto('command', <q-bang>, <q-args>) 
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

function! ListFunctions(bang)
    lvimgrep /^\s*fun\%[ction]/gj %
    let loclist = getloclist(0)
    call map(loclist, 'get(v:val, "text", "")')  
    call map(loclist, 'matchstr(v:val, ''^\s*fun\%[ction]!\=\s*\zs.*\ze\s*('')')
    if a:bang == "!"
	call sort(loclist)
    endif
    return join(<SID>PrintTable(loclist, 2), "\n")
endfunction
command! -bang ListFunctions 	:echo ListFunctions(<q-bang>)

function! ListCommands(bang)
    lvimgrep /^\s*com\%[mmand]/gj %
    let loclist = getloclist(0)
    call map(loclist, 'get(v:val, "text", "")')  
    call map(loclist, 'substitute(v:val, ''^\s*'', '''', '''')')
    if a:bang == "!"
	call sort(loclist)
    endif
    return join(loclist, "\n")
endfunction
command! -bang ListCommands 	:echo ListCommands(<q-bang>)
" Print table tools:
" {{{
function! <SID>FormatListinColumns(list,s)
    " take a list and reformat it into many columns
    " a:s is the number of spaces between columns
    " for example of usage see atplib#PrintTable
    let max_len=max(map(copy(a:list), 'len(v:val)'))
"     let g:list=a:list
"     let g:max_len=max_len+a:s
    let new_list=[]
    let k=&l:columns/(max_len+a:s)
"     let g:k=k
    let len=len(a:list)
    let column_len=len/k
    for i in range(0, column_len)
	let entry=[]
	for j in range(0,k)
	    call add(entry, get(a:list, i+j*(column_len+1), ""))
	endfor
	call add(new_list,entry)
    endfor
    return new_list
endfunction 
" Take list format it with atplib#FormatListinColumns and then with
" atplib#Table (which makes columns of equal width)
function! <SID>PrintTable(list, spaces)
    " a:list 	- list to print
    " a:spaces 	- nr of spaces between columns 

    let list = atplib#FormatListinColumns(a:list, a:spaces)
    let nr_of_columns = max(map(copy(list), 'len(v:val)'))
    let spaces_list = ( nr_of_columns == 1 ? [0] : map(range(1,nr_of_columns-1), 'a:spaces') )

    let g:spaces_list=spaces_list
    let g:nr_of_columns=nr_of_columns
    
    return atplib#Table(list, spaces_list)
endfunction
"}}}
doc/ftpdev.txt	[[[1
148
*ftpdev.txt* 			For Vim version 7	Last change: 9 April 2011

		    A help file for <F>ile <T>ype <P>lugin <DEV>elopemnt  (ver 3)
				by Marcin Szamotulski
			    mszamot [AT] gmail [DOT] com
			----------------------------------------

This file type plugin provides some additional functions and commands which
help writing and vim plugins. Especialy big ones. Here is the list of
available commands and maps with some explanation:

NEWS							*ftpdev-news*	

>
 version 3
<
In the version 3 there are two new commands:
:ListFunctions
:ListCommands
	They list functions and commands defined in the current file. 

:Function {function-name}
	Has completion for names of functions found in the project.
:Commands {command-name}
	Has completion for names of commands found in the project.

	Both completions add \> at the end of the function/command name to
	make the match exact. This is done by completion function and not,
	silently, by the commands (this makes a difference).
	
>
 version 2
<
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

	There is completion for {fname}s.

:Command[!] {cname}					*ftpdev-:Command*
	This finds command which name matches {fname} vim pattern under the
	|ftpdev-g:ftplugin_dir| {cname> has the same syntax as {fname}. The
	bang works as in |ftpdev-:Function|

	There is completion for {cname}s.

:Variable[!] {vname}					*ftpdev-:Variable*
	This finds variable definition mathing {vname}. {vname} has to contain
	the g: b: s: t: prefix if the variable has it. The bang works as in
	|ftpdev-:Function|

:Map[!] {maplhs}					*ftpdev-:Map*
	This finds maps which lhs matches {maplhs} vim pattern. The bang works as in
	|ftpdev-:Function|

							*ftpdev-gn*
							*ftpdev-gN*
							*ftpdev-:S*
:S /{pattern}/
map gn
map gN
	This function make a search of {pattern} in the current function (it
	can wrap around the end of function. The argument {pattern} is a vim
	pattern passed to |vimgrep|.

	The pattern is added to search history and copied to the @/ register,
	thus you can re use it with |n| and |N| commands to search globaly or
	gn and gN to search in the scope of current function.

	Note that you can first search with the vim standard |/| or |?| and
	then use |ftpdev-gn| and |ftpdev-gN|.

							*ftpdev-:PluginDir*
:PluginDir {directory}	
	This sets the variable |ftpdev-g:ftplugin_dir|. It has dir type
	completion.

							*ftpdev-:Pgrep*
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
