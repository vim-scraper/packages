" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/vim_ftpdev.vim	[[[1
472
" Title:  Vim filetype plugin file
" :
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
if !exists("g:ftplugin_installdir")
    let g:ftplugin_installdir=split(&runtimepath,",")[0]
endif
if !exists("g:ftplugin_notinstall")
    let g:ftplugin_notinstall=['Makefile', '.*\.tar\.\%(bz2\|gz\)$', '.*\.vba$']
endif
if exists("g:ftplugin_ResetPath") && g:ftplugin_ResetPath == 1
    au! BufEnter * let &l:path=g:ftplugin_dir
else
    function! FTPLUGIN_AddPath()
	let path=map(split(&path, ','), "fnamemodify(v:val, ':p')")
	if index(path,fnamemodify(g:ftplugin_dir, ":p")) == -1
	    let &l:path=( len(path)==0 ? g:ftplugin_dir : &path.",".g:ftplugin_dir )
	endif
    endfunction
    exe "au! BufEnter ".g:ftplugin_dir."* call FTPLUGIN_AddPath()"
    exe "au! VimEnter * call FTPLUGIN_AddPath()"
endif
try
function! Goto(what,bang,...)
    let g:a	= (a:0 >= 1 ? a:1 : "")
    let pattern = (a:0 >= 1 ? 
		\ (a:1 =~ '.*\ze\s\+\d\+$' ? matchstr(a:1, '.*\ze\s\+\d\+$') : a:1)
		\ : 'no_arg') 
    let line	= (a:0 >= 1 ? 
		\ (a:1 =~ '.*\ze\s\+\d\+$' ? matchstr(a:1, '.*\s\+\zs\d\+$') : 0) 
		\ : 0)
    	let g:pattern_arg 	= pattern
	let g:line_arg		= line
    " Go to a:2 lines below
    let g:line = line
    let grep_flag = ( a:bang == "!" ? 'j' : '' )
    if a:what == 'function'
	let pattern		= '^\s*fu\%[nction]!\=\s\+\%(s:\|<\csid>\)\=' .  ( a:0 >=  1 ? pattern : '' )
    elseif a:what == 'command'
	let pattern		= '^\s*com\%[mand]!\=\%(\s*-buffer\s*\|\s*-nargs=[01*?+]\s*\|\s*-complete=\S\+\s*\|\s*-bang\s*\|\s*-range=\=[\d%]*\s*\|\s*-count=\d\+\s*\|\s*-bar\s*\|\s*-register\s*\)*\s*'.( a:0 >= 1 ? pattern : '' )
    elseif a:what == 'variable'
	let pattern 		= '^\s*let\s\+' . ( a:0 >=  1 ? pattern : '' )
    elseif a:what == 'maplhs'
	let pattern		= '^\s*[cilnosvx!]\=\%(nore\)\=m\%[ap]\>\s\+\%(\%(<buffer>\|<silent>\|<unique>\|<expr>\)\s*\)*\(<plug>\)\=' . ( a:0 >= 1 ? pattern : '' )
    elseif a:what == 'maprhs'
	let pattern		= '^\s*[cilnosvx!]\=\%(nore\)\=m\%[ap]\>\s+\%(\%(<buffer>\|<silent>\|<unique>\|<expr>\)\s*\)*\s\+\<\S\+\>\s\+\%(<plug>\)\=' . ( a:0 >= 1 ? pattern : '' )
    else
	let pattern 		= '^\s*[ci]\=\%(\%(nore\|un\)a\%[bbrev]\|ab\%[breviate]\)' . ( a:0 >= 1 ? pattern : '' )
    endif
    let g:pattern		= pattern
    let filename		= join(map(split(globpath(g:ftplugin_dir, '**/*vim'), "\n"), "fnameescape(v:val)"))

    let error = 0
    try
	exe 'silent! vimgrep /'.pattern.'/' . grep_flag . ' ' . filename
    catch /E480:/
	echoerr 'E480: No match: ' . pattern
	let error = 1
    endtry

    if len(getqflist()) >= 2
	clist
    endif
    if !error
	exe 'silent! normal zO'
	exe 'normal zt'
    endif

    " Goto lines below
    if line
	exe "normal ".line."j"
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
    try
	exe 'lvimgrep /^\s*fun\%[ction]/gj '.filename
    catch /E480:/
    endtry
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
    try
	exe 'lvimgrep /^\s*com\%[mand]/gj '.filename
    catch /E480:/
    endtry
    let loclist = getloclist(0)
    call setloclist(0, saved_loclist)
    call map(loclist, 'get(v:val, "text", "")')  
    call map(loclist, 'matchstr(v:val, ''^\s*com\%[mand]!\=\(\s*-buffer\s*\|\s*-nargs=[01*?+]\s*\|\s*-complete=\S\+\s*\|\s*-bang\s*\|\s*-range=\=[\d%]*\s*\|\s*-count=\d\+\s*\|\s*-bar\s*\|\s*-register\s*\)*\s*\zs\w*\>\ze'')')
    call map(loclist, 'v:val.''\>''')
    return join(loclist, "\n")
endfunction
function! MapRhsCompl(A,B,C)
    let saved_loclist=getloclist(0)
    let filename	= join(map(split(globpath(g:ftplugin_dir, '**/*vim'), "\n"), "fnameescape(v:val)"))
    try
	exe 'lvimgrep /^\s*[cilnosvx!]\=\%(nore\)\=m\%[ap]\>/gj '.filename
    catch /E480:/
    endtry
    let loclist = getloclist(0)
    call setloclist(0, saved_loclist)
    call map(loclist, 'get(v:val, "text", "")')  
    call map(loclist, 'matchstr(v:val, ''^\s*[cilnosvx!]\=\%(nore\)\=m\%[ap]\>\s\+\%(\%(<buffer>\|<silent>\|<unique>\|<expr>\)\s*\)*\(<plug>\)\=\zs.*'')')
    call map(loclist, 'matchstr(v:val, ''\S\+\s\+\zs.*'')')
    call map(loclist, 'escape(v:val, "[]")')
    return join(loclist, "\n")
endfunction
function! MapLhsCompl(A,B,C)
    let saved_loclist=getloclist(0)
    let filename	= join(map(split(globpath(g:ftplugin_dir, '**/*vim'), "\n"), "fnameescape(v:val)"))
    try
	exe 'lvimgrep /^\s*[cilnosvx!]\=\%(nore\)\=m\%[ap]\>/gj '.filename
    catch /E480:/
    endtry
    let loclist = getloclist(0)
    call setloclist(0, saved_loclist)
    call map(loclist, 'get(v:val, "text", "")')  
    call map(loclist, 'matchstr(v:val, ''^\s*[cilnosvx!]\=\%(nore\)\=m\%[ap]\>\s\+\%(\%(<buffer>\|<silent>\|<unique>\|<expr>\)\s*\)*\(<plug>\)\=\zs\S*\ze'')')
    call map(loclist, 'escape(v:val, "[]")')
    return join(loclist, "\n")
endfunction
command! -buffer -bang -nargs=? -complete=custom,CommandCompl Command 	:call Goto('command', <q-bang>, <q-args>) 
command! -buffer -bang -nargs=?  			Variable 	:call Goto('variable', <q-bang>, <q-args>) 
command! -buffer -bang -nargs=? -complete=custom,MapLhsCompl MapLhs 		:call Goto('maplhs', <q-bang>, <q-args>) 
command! -buffer -bang -nargs=? -complete=custom,MapRhsCompl MapRhs 		:call Goto('maprhs', <q-bang>, <q-args>) 

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
function! <SID>GetSearchArgs(Arg,flags)
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

    let [ pattern, flag ] = <SID>GetSearchArgs(a:Arg, 'bcenpswW')
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
command! -buffer -nargs=*	S 	:call Search(<q-args>) | let v:searchforward = ( <SID>GetSearchArgs(<q-args>, 'bcenpswW')[1] =~# 'b' ? 0 : 1 )
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
    let filename	= join(filter(map(split(globpath(g:ftplugin_dir, '**/*'), "\n"), "fnameescape(v:val)"),"!isdirectory(v:val)"))
    try
	execute "vimgrep " . a:vimgrep_arg . " " . filename 
    catch /E480:/
	echohl ErrorMsg
	redraw
	echo "E480: No match: ".a:vimgrep_arg
	echohl Normal
    endtry
endfunction
catch /E127:/
endtry
command! -nargs=1 Pgrep		:call Pgrep(<q-args>)

function! ListFunctions(bang)
    try
	lvimgrep /^\s*fun\%[ction]/gj %
    catch /E480:/
	echohl ErrorMsg
	redraw
	echo "E480: No match: ".a:vimgrep_arg
	echohl Normal
    endtry
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
    try
	lvimgrep /^\s*com\%[mmand]/gj %
    catch /E480:/
	echohl ErrorMsg
	redraw
	echo "E480: No match: ".a:vimgrep_arg
	echohl Normal
    endtry
    let loclist = getloclist(0)
    call map(loclist, 'get(v:val, "text", "")')  
    call map(loclist, 'substitute(v:val, ''^\s*'', '''', '''')')
    if a:bang == "!"
	call sort(loclist)
    endif
    let cmds = []
    for raw_cmd in loclist 
	let pattern = '^\s*com\%[mand]!\=\%(\s*-buffer\s*\|\s*-nargs=[01*?+]\s*\|\s*-complete=\S\+\s*\|\s*-bang\s*\|\s*-range=\=[\d%]*\s*\|\s*-count=\d\+\s*\|\s*-bar\s*\|\s*-register\s*\)*\s*\zs\w*\ze'
	call add(cmds, matchstr(raw_cmd, pattern))
    endfor

    return join(cmds, "\n")
endfunction
command! -bang ListCommands 	:echo ListCommands(<q-bang>)

try
function! Edit(cmd,args)
    
    let edit_args = matchstr(a:args, '\zs\(++\(\w\|=\)\+\s*\)*\(\s*+\S*\s*\)*')
    let file = strpart(a:args,matchend(a:args, '\zs\(++\(\w\|=\)\+\s*\)*\(\s*+\S*\s*\)*'))
    let g:file = file
    if edit_args !~# '++bin' && a:args =~# '++bin'
	echoerr "[ftpdev:] Edit arg error: a:args contains ++bin, and edit_args doesn't"
	return
    endif
    if file == ""
	return
    endif
    let files = split(globpath(g:ftplugin_dir, '**/'.file), "\n")
    if len(files) == 0
	return
    elseif len(files) == 1
	execute a:cmd." ".edit_args." ".files[0]
    else
	let files_s = []
	let i=1
	for file in files 	
	    call add(files_s, i.". ".file)
	    let i+=1
	endfor
	let input = inputlist(['Which file to edit? write number and hit <enter>, nothing to exit ']+files_s)
	if input >= 1 && input <= len(files)
	    execute a:cmd." ".edit_args." ".files[input-1]
	endif
    endif
endfunction
catch E127:
endtry
function! EditCompl(A,B,C)
    let s:pat = a:A
    let list=filter(split(globpath(g:ftplugin_dir, "**"), "\n"), '!isdirectory(v:val)')
    call remove(list, index(list,expand("%:p")))
    let list_orig = copy(list)
    " This is not the best, I should check if the file name is unique if not
    " add portion of the path which distinguishes them:
    call map(list, 'fnamemodify(v:val, ":t")')
    " Anyway the Edit function will show full path of files to chose from:
    " So we can filter out double entries:
    for file in list
	if count(list,file) > 1 
	    call remove(list,index(list,file))
	endif
    endfor
"     let file = strpart(a:C,matchend(a:C, '\(\(.*++\(\w\|=\)\+\s*\)\=\&\(.*+\S\+\s*\)\=\)'))
"     call filter(list, 'v:val =~# file')
    call filter(list, 'v:val =~# a:A')
    if list == []
	" If the list was empty match path relative to g:ftpdev_dir.
	let list=list_orig
	let cwd = getcwd()
	exe "lcd ".g:ftplugin_dir
	call map(list, 'fnamemodify(v:val, ":.")')
	exe "lcd ".cwd
	call filter(list, 'v:val =~# a:A')
    endif

    call sort(list)
    function! MyCompare(i1,i2)
	let pat = ( s:pat =~ '^\^' ? substitute(s:pat,'^\^', '','')  : s:pat )
	let n1  = match(a:i1,pat)
	let n2  = match(a:i2,pat)
	return n1 == n2 ? 0 : n1 > n2 ? 1 : -1
    endfunction
    call sort(list, "MyCompare")
    redraw
    return list
"     return join(list, "\n")
endfunction
command! -nargs=1 -complete=customlist,EditCompl Edit		:call Edit("edit",<q-args>)
command! -nargs=1 -complete=customlist,EditCompl Split		:call Edit("split",<q-args>)
command! -nargs=1 -complete=customlist,EditCompl Vsplit		:call Edit("vsplit",<q-args>)
command! -nargs=1 -complete=customlist,EditCompl Tabe		:call Edit("tabedit",<q-args>)
command! -nargs=1 -complete=customlist,EditCompl Diffsplit	:call Edit("diffsplit",<f-args>)

nmap	Gn	:call searchpair('^[^"]*\<\zsif\>', '^[^"]*\<\zselse\%(if\)\=\>', '^[^"]*\<\zsendif\>')<CR>
nmap	GN	:call searchpair('^[^"]*\<\zsif\>', '^[^"]*\<\zselse\%(if\)\=\>', '^[^"]*\<\zsendif\>', 'b')<CR>

function! <SID>Install(bang)

    let cwd = getcwd()
    exe 'lcd '.g:ftplugin_dir
    
    if a:bang == "" 
	" Note: this returns non zero list if the buffer is loaded
	" ':h getbufline()'
	let file_name = fnamemodify(bufname(""), ":.")
	let file		= getbufline( '%', '1', '$')
	call writefile( file, $HOME . "/.vim/" . file_name)
    else
	for file in filter(split(globpath(g:ftplugin_dir, "**"), "\n"), "!isdirectory(v:val) && <SID>Index(g:ftplugin_notinstall, fnamemodify(v:val, ':.')) == -1")
	    echo file
	    if bufloaded(file)
		let file_list		= getbufline( file, '1', '$')
	    else
		let file_list		= readfile( file)
	    endif
	    let file_name = fnamemodify(file, ":.")
	    call writefile(file_list, substitute(g:ftplugin_installdir, '\/$', '', '')."/".file_name)
	endfor
    endif
    exe "lcd ".cwd
endfunction
function! <SID>Index(list, pattern)
    let ind = -1
    for element in a:list
	let ind += 1
	if element =~ a:pattern || element == a:pattern
	    return ind
	endif
    endfor
    return -1
endfunction
command! -bang Install 	:call <SID>Install(<q-bang>)

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
245
*ftpdev.txt* 			For Vim version 7	Last change: 23 April 2011


		    A help file for <F>ile <T>ype <P>lugin <DEV>elopemnt  (ver. 5.2)
				by Marcin Szamotulski
			    mszamot [AT] gmail [DOT] com
			----------------------------------------

This file type plugin provides some additional functions and commands which
help writing and vim plugins. Especially big ones. Here is the list of
available commands and maps with some explanation:

======
NEWS							*ftpdev-news*	

>
 version 5.2
<
 |ftpdev-:Pgrep| searches in all files under |ftpdev-g:ftplugin_dir|, not only vim
 scripts, this is useful if some of the scripts are written in other language
 (python, perl, ruby).

:Install[!] 						|ftpdev-:Install|
 	Without bang "!": will copy the current buffer to the location
	under |g:ftplugin_installdir| be default it is first path that appear in
	'runtimepath' vim option.
	With bang "!": it will install all the files found under
	|g:ftplugin_dir| except the files in the list |g:ftplugin_notinstall|.

	You can use this command in an autocommand for certain files: >
		au BufWritePost <some_file>	:Install
<
							|ftpdev-g:ftplugin_notinstall|
g:ftplugin_notinstall=['Makefile', '.*\.tar\.\%(bz2\|gz\)$', '.*\.vba$']
	This is list of patterns or file names relative to |g:ftplugin_dir|
	which will not be installed by |ftpdev-:Install| command.

							|ftpdev-g:ftplugin_installdir|
g:ftplugin_installdir=split(&runtimepath,",")[0]
	Directory name where to |ftpdev-:Install| files.

======
CONFIGURATION						*ftpdev-configure* 

							*ftpdev-g:ftplugin_dir*
There is one variable which needs to be configured >
 g:ftplugin_dir
< This is the directory which will be searched for files. If you set this to
your ~/.vim/ftplugin directory where you have lots of scripts this plugin
might be a bit slow. Each plugin that I develop has its own directory (you
probably use some version control system as I do) then this variable is set
in my vimrc file via an autocommand: >
	au BufEnter /plugin_dev_dir/*		let g:ftplugin=<plugin_dev_dir>
<
							*ftpdev-:PluginDir*
:PluginDir {dir}	
	This sets value of the variable |ftpdev-g:ftplugin_dir| to {dir}. It
	has dir type completion.

 							*ftpdev-g:ftplugin_ResetPath*
 |g:ftplugin_ResetPath| is set to 1 it will set |path|=g:ftplugin_dir, if not
 set or equal 0 it will add g:ftplugin_dir to |path| (only if the path already
 does not contain it). 
 This makes the use of |edit|, |split|, |vsplit|, |diffsplit| a lot nicer.

							*ftpdev-g:ftplugin_notinstall*
g:ftplugin_notinstall=['Makefile', '.*\.tar\.\%(bz2\|gz\)$', '.*\.vba$']
	This is list of patterns or file names relative to |g:ftplugin_dir|
	which will not be installed by |ftpdev-:Install| command.

							*ftpdev-g:ftplugin_installdir*
g:ftplugin_installdir=split(&runtimepath,",")[0]
	Directory name where to |ftpdev-:Install| files.


TIP ~

 It might be good idea to put in your vimrc file: >
     au BufEnter  {path_to_project_dir}/* source ~/.vim/ftplugin/vim_ftdev.vim
<In this way you will get access to |ftpdev-:Function|, |ftpdev-:Command|, ...
 commands even in files of other file type than "vim" (for example in "txt"
 files).

======
COMMANDS AND MAPS					*ftpdev*

SEARCHING COMMANDS ~
:Function[!] {fname} [line]				*ftpdev-:Function*
	This finds function with name matching {fname} vim pattern under the
	|ftpdev-g:ftplugin_dir| directory. It uses the internal grep, i.e.
	|vimgrep|. {fname} should be as the first argument of |vimgrep| except
	it doesn't have to be put between /.../ .

	{fname} should not begin with <SID> or 's:' even if the function name has
	it.

	The bang vimgrep is supplied with the j switch, i.e. only quick fix
	list will be filled with matches.

	There is a completion for {fname}s.

	If [line] is non zero, go [lines] below. This is especially useful
	when you are debugging a script and you got en error in function
	{fname} at line [line].

:ListFunctions						 *ftpdev-:ListFunctions*
	List functions in defined in the current buffer.

:Command[!] {cname} [line]				*ftpdev-:Command*
	This finds command which name matches {fname} vim pattern under the
	|ftpdev-g:ftplugin_dir| {cname> has the same syntax as {fname}. The
	bang works as in |ftpdev-:Function|

	There is a completion for {cname}s.
	The [line] argument is the same as in |ftpdev-:Function|.

:ListCommands						 *ftpdev-:ListCommands*
	List commands in defined in the current buffer.

:Variable[!] {vname} [line]				*ftpdev-:Variable*
	This finds variable definition matching {vname}. {vname} has to contain
	the g: b: s: t: prefix if the variable has it. The bang works as in
	|ftpdev-:Function|
	The [line] argument is the same as in |ftpdev-:Function|.

:MapLhs[!] {maplhs} [line]				*ftpdev-:MapLhs*
	This finds maps which lhs matches {maplhs} vim pattern. The bang works as in
	|ftpdev-:Function|
	The [line] argument is the same as in |ftpdev-:Function|.

:MapRhs[!] {maprhs} [line]				*ftpdev-:MapRhs*
	This finds maps which rhs matches {maprhs} vim pattern. The bang works as in
	|ftpdev-:Function|
	The [line] argument is the same as in |ftpdev-:Function|.
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
	thus you can re use it with |n| and |N| commands to search globally or
	gn and gN to search in the scope of current function.

	Note that you can first search with the vim standard |/| or |?| and
	then use |ftpdev-gn| and |ftpdev-gN|.

							*ftpdev-:Pgrep*
:Pgrep /{pattern}/[j][g]
	This makes |vimgrep| in all files under |ftpdev-g:ftplugin_dir|.  The
	argument syntax is the same as for |vimgrep|.
	

Remember: Vim has its own vim.vim ftplugin which defines: 
	]] 	(next beginning of a function), 
	][ 	(next end of a function), 
	[[ 	(previous beginning of a function), 
	[] 	(previous end of a function).
	
nmap Gn							*ftpdev-Gn*
nmap GN							*ftpdev-GN*
	Go to next/previous if/else/elseif/endif pair (this, unlike |n| and |N|
	vim maps, doesn't depend on v:searchforward). These maps are defined
	by: >
 nmap	Gn	:call searchpair('^[^"]*\<\zsif\>', '^[^"]*\<\zselse\%(if\)\=\>', '^[^"]*\<\zsendif\>')<CR>
 nmap	GN	:call searchpair('^[^"]*\<\zsif\>', '^[^"]*\<\zselse\%(if\)\=\>', '^[^"]*\<\zsendif\>', 'b')<CR>
<	

EDITING A FILE ~
:Edit [++opt] [+cmd] {file}				*ftpdev-:Edit*
:Tabe [++opt] [+cmd] {file}
	Like |:edit| or |:tabe| vim command. See |++opt| and |+cmd|. If |++bin| is given
	and then forgot returns with an error (while this should not happen,
	it is for safety reasons).
	
	The {file} argument has a completion for files under the
	|g:ftpdev_dir| directory (using |globpath()| vim function). The
	completion function accepts {file} to be a pattern, for example >
		:Edit +10  \.txt$<tab>
<	Will list all txt files. If none file names match, path relative to
	|g:fptlugin_dir| are tried. The completions with {file} containing
	spaces might not work right.
		
:Split [++opt] [+cmd] {file}				*ftpdev-:Split*
	Like |:split| vim command. See |++opt| and |+cmd|. If |++bin| is given and then
	forgot returns with an error (while this should not happen, it is for
	safety reasons).

	For {file} argument completion read |ftpdev-:Edit|. 

:Vsplit [++opt] [+cmd] {file}				*ftpdev-:Vsplit*
	Like |:vsplit| vim command. See |++opt| and |+cmd|. If |++bin| is given and then
	forgot returns with an error (while this should not happen, it is for
	safety reasons).

	For {file} argument completion read |ftpdev-:Edit|. 

:Diffsplit {file}					*ftpdev-:Diffsplit*
	Like |:diffsplit| vim command. See |++opt| and |+cmd|. If |++bin| is given and then
	forgot returns with an error (while this should not happen, it is for
	safety reasons).

	For {file} argument completion read |ftpdev-:Edit|. 

INSTALLING ~

:Install[!] 						*ftpdev-:Install*
	Without bang "!": it will copy the current buffer to the location
	under |g:ftplugin_installdir| be default it is first path that appear
	in 'runtimepath' vim option.
	With bang "!": it will install all the files found under
	|g:ftplugin_dir| except the files in the (file/pattern) list |g:ftplugin_notinstall|.


Happy vimming :)	

================================================================================
COPY RIGHTS						*ftpdev-copy-rights*


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


vim:tw=75:ts=8:ft=help:norl:
