" Vim plugin  Save all written files into file ~/.vim/recent_files.  Add
"             command :Recent to open a buffer listing all edited files.
" File: recentern.vim
" Last Change:	2007 Jan 18
" Version:	9.2
" Author:	Andy Wokula <anwoku@yahoo.de>
"
" Credits:
" Based on #1228 recent.vim : List recent edited files
"	 by Tien Hai Nguyen <thnguyen@dental-on-line.fr>
"	 http://vim.sourceforge.net/scripts/script.php?script_id=1228
" Suggestions by:
"	 Ilia N Ternovich   (hence the name recen-tern.vim;)
" See also: #207, #521
"
" Requirements:
" - Vim7, nothing else (GUI; Python, Perl etc.)
" - OS: Tested on but not restricted to Win32

" Installation:
" - put file in plugin folder or :source it directly
"   :help add-plugin
" - (optional) see "Customization:" below
"
" Usage:
"   Edit some file, then write it.  This adds the file name to the list of
"   recent files.  The list is stored as a file, default location is
"   "~/.vim/recent_files" (first entry of 'rtp' used).
"
"   Use the :Recent command to open the list file.  Within its buffer, move
"   the cursor to a filename and press one of the following keys: Enter, gf,
"   Ctrl­W_f, Ctrl-W_gf or mouse double click.
"
"   For Enter and mouse double click it depends on the Winmode if the file
"   is opened in the current window, in a new window or in a new tabpage.
"   The other keys behave like described in the :help.  All keys reuse an
"   existing window of a file if possible.
"
"   You can use :RememberFile to manually add the current file to the list.
"   This command is also useful to jump to an existing entry (the command
"   uses :Recent to open the list window and then edits there to add an
"   entry).
"
"   Winmode
"   Use :RecentWinmode to cycle through the Winmodes "thiswin", "newwin" and
"   "newtab" (self-explanatory), this has instant effect on :Recent, Enter
"   and mouse double click.
"
"   Per default, "thiswin" and "newwin" reuse the current tabpage - the same
"   where recent_files is in.  With :RecentReusetab you can toggle an option
"   to reuse the last accessed tabpage.
"
"   Note: I recommend using :Recent to go the file list.
"
"	Else the script might guess the last accessed tabpage wrong, for
"	example.
"
"   (see more in the Features-section below)
"
" Features:
" - files are remembered if they are written - and only then, viewed files
"   are not remembered
" - uses windows and tabpages (depending on g:RecentWinmode)
" - reuses window and tabpage for existing buffer
" - no limit in number of remembered files
" - you are allowed to edit recent_files manually
" - files that cannot be read (on Enter, gf, etc.) are removed from list;
"   use undo to get them back
" - new entries added at top
" - syntax highlighting of file names (suffix based)
" - always jumps to last known file position (mark "), explicitly done for
"   :edit
" - patterns for files to be ignored (patterns like in autocommands)
" - tries to write recent_files only when necessary
" - for Winmodes "thiswin" and "newwin": boolean option "g:RecentReusetab"
"   to reuse last accessed tab - before "thiswin" or "newwin" is applied, go
"   to tabpage that was current before :Recent; can be toggled with
"   :RecentReusetab
"
" Features NOT Here: (and not on TODO list)
" - support for Vim6 (has no tabpages ...)
" - GUI menu
" - nice layout of entries (filename to the left, dir to the right ?)
" - customizable position of new window and new tabpage
" - sort functions, e.g. sort by name or extension  (use :sort for this)
" - autoadd any visited file to the list (use the other plugins out there)
" - filename completion in command-line based on filelist (v10 of the plugin
"   has it, but searching for a file in the list is quick and useful too)
" - mappings (you should map the commands yourself
"	:nn \r :Recent<cr>
"   for example)
" - open many files at the same time (use sessions)
"
" Customization: -> .vimrc, given values are the defaults!
" Where to store the file list:
"   :let g:RecentFullname = matchstr(&rtp,'[^,]*').'/recent_files'
"
" Option to show recent_files at startup:
"   :let g:RecentStartup = 1
"
" Default action for Enter and mouse double click in recent_files:
"   :let g:RecentWinmode = "thiswin"
"   Possible values:
"	"thiswin" (like gf)
"	"newwin" (like Ctrl-W_f)
"	"newtab" (like Ctrl-W_gf)
"   Also used for the  :Recent  command (if you want Enter to open a new
"   tab, then you must also want :Recent to open a new tab)
"   Note: Value "thiswin" splits window if current buffer cannot be
"	abandoned (looks at 'modified'; 'hidden' or 'autowrite' ignored)
"
" Syntax Highlighting of files with given suffixes:
"   :let g:RecentTypes =  ".cpp,.h,.pas,.java|.txt|.vim"
"   :let g:RecentHlGroups = "Constant,Identifier,Statement,PreProc,Type,Special"
"   Effect:
"	File suffix	      Highlighting
"	.cpp,.h,.pas,.java -> Constant
"	.txt		   -> Identifier
"	.vim		   -> Statement
"
" Files to be ignored (autocmd pattern):
"   :let g:RecentIgnore = "*.txt,*.svn,*.cvs,*.bak,*~"
"
"   Note: the four global variables you get with
"	    :let g:Recent<C-A><Enter>
"	can be changed at any time during session!
"
" Format: (of recent_files)
"   list of full qualified filenames, one per line, can be relative to the
"   home directory ('~' prepended) if possible, spaces in and around
"   filenames allowed (latter trimmed), other data not allowed (will be
"   removed if pressing Enter on it)
"
" Bugs:
" - recent_files not always reloaded if manually opened
"   Workaround: always use :Recent
"   (could solve it but do not like adding autocmds and autocmds ...)
" - curious: if line under cursor is blank, Enter goes to buffer with
"   filename in or below current directory; only if buffer (besides
"   recent_files) exists and match is unique
"
" TODO
" - help file

if exists("loaded_recentern") || &cp
    finish
endif
let loaded_recentern = 1

if v:version < 700
    " sorry, Vim7 required
    echoerr expand("<sfile>").": Sorry, Vim7 required"
    finish
endif

let s:save_cpo = &cpo
set cpo&vim

" Script Global Variables:
let s:WinModes = ["thiswin", "newwin", "newtab"]
let s:ListChanged = 0

" Check User Options:
if !exists("g:RecentFullname")
    " per default use <first-entry-of-runtimepath>.'/recent_files'
    let s:RecentName = "recent_files"
    let s:RecentFullname = matchstr(&rtp, '[^,]*').'/'.s:RecentName
else
    let s:RecentFullname = g:RecentFullname
    let s:RecentName = fnamemodify(s:RecentFullname, ":t")
    unlet g:RecentFullname
endif
" finish script if cannot write to recent_files
if filereadable(s:RecentFullname)
	    \ ? !filewritable(s:RecentFullname)
	    \ : writefile([], s:RecentFullname) < 0
    echoerr "s:RecentFullname='".s:RecentFullname."' not writeable, abort."
    finish
    " writefile may also throw a vim exception (das ist Jacke wie Hose;)
endif
if !exists("g:RecentStartup")
    let g:RecentStartup = 1
endif
if !exists("g:RecentTypes")
    let g:RecentTypes = ".cpp,.h,.pas,.java|.txt|.vim"
endif
if !exists("g:RecentHlGroups")
    let g:RecentHlGroups = "Constant,Identifier,Statement,PreProc,Type,Special"
endif
if !exists("g:RecentIgnore")
    let g:RecentIgnore = '*.txt,*.svn,*.cvs,*.bak,*~'
endif
if !exists("g:RecentWinmode")
    let g:RecentWinmode = "thiswin"
else
    if index(s:WinModes, g:RecentWinmode) < 0
	echoerr 'Warning: g:RecentWinmode must be in' string(s:WinModes)
		    \.", revert to 'thiswin'"
	let g:RecentWinmode = "thiswin"
    endif
endif
if !exists("g:RecentReusetab")
    let g:RecentReusetab = 0
endif

augroup Recentfiles
    au!
    " set options when reading the file list:
    exec "au BufRead" s:RecentName "call s:RecentfileOptions()"
    " before writing, check consistency
    exec "au BufWritePre" s:RecentName "call s:Crfbwti()"
    exec "au BufRead" g:RecentIgnore "let b:RecentIgn = 1"
    " add file to the list, if it is written:
    au BufWritePost * call s:RecentfilesAdd(expand("<afile>:p"))
augroup End
unlet g:RecentIgnore

" Prepare For Startup:
if g:RecentStartup
    " starting without arguments loads the list of recent files
    if argc()==0 && filereadable(s:RecentFullname)
	" not now, wait until VimEnter:
	au Recentfiles VimEnter * nested Recent
    endif
endif
unlet g:RecentStartup


" commands used together with g:RecentWinmode
" edit file:
let s:EditCmd = {"thiswin" : "edit", "newwin" : "new", "newtab" : "999tabedit"
	    \, "thiswinlis" : "edit", "newwinlis" : "top new", "newtablis": "0tabedit"
	    \, "thiswinmod" : "split", "newwinmod" : "top new", "newtabmod" : "0tabedit"}
" goto buffer:
let s:BufCmd = {"thiswin" : "buf", "newwin" : "sbuf", "newtab" : "999tab sbuf"
	    \, "thiswinlis" : "buf", "newwinlis" : "top sbuf", "newtablis" : "0tab sbuf"
	    \, "thiswinmod" : "sbuf", "newwinmod" : "top sbuf", "newtabmod" : "0tab sbuf"}
" line 1/3: open a file from recent_files (with <Enter>, gf, etc.)
" line 2/3: open recent_files, if can reuse current win
" line 3/3: open recent_files, if current buffer modified

" set options for the recent_files buffer (after BufRead)
function s:RecentfileOptions()
    no <buffer><silent> gf :call <sid>Recentgf("thiswin")<cr>
    no <buffer><silent> <c-w>f :call <sid>Recentgf("newwin")<cr>
    no <buffer><silent> <c-w>gf :call <sid>Recentgf("newtab")<cr>
    no <buffer><silent> <cr> :call <sid>Recentgf(g:RecentWinmode)<cr>
    if has("mouse")
	map <buffer> <2-leftmouse> <cr>
    endif
    setl number
    setl noswapfile nobuflisted autoread
    call s:SetSyntax()
    let b:RecentIgn = 1
endfunction

" Syntax settings for recent_files (after BufRead)
function s:SetSyntax()
    syn clear
    let hlgroups = split(g:RecentHlGroups, ",")
    let hlglen = len(hlgroups)
    if !hlglen|return|endif
    let ftgroups = split(g:RecentTypes, "|")
    let i = 0
    for ftg in ftgroups
	if ftg==""|cont|endif
	let gn = "recentGroup".i
	let pat = substitute(ftgroups[i],',','\\|','g')
	exec "syn match" gn '/^\V\.\*\%('.pat.'\)\$/'
	exec "hi link" gn hlgroups[i%hlglen]
	let i+=1
    endfor
endfunction

" return [number of tabpage, number of win in tabpage] for an existing
" buffer (first found)
function s:GetTabWin(bufnr)
    " there is no direct mapping, we have to search through the tabpages
    let tabpcount = tabpagenr("$")  " first tabpage has no 1
    let tabnr = 1
    while tabnr <= tabpcount
	let bufwnr = index(tabpagebuflist(tabnr), a:bufnr)
	if bufwnr >= 0
	    return [tabnr, bufwnr+1]
	endif
	let tabnr += 1
    endwhile
    return [-1, -1]
endfunction

" edit filename, reuse buffer and tabpage/window of filename if possible, if
" cannot reuse window, reuse window of recent_files = current window
function s:Visit(filename, winmode)
    " filename: must be full qualified file name
    " winmode: "", "thiswin", "newwin" or "newtab"
    let winmode = (a:winmode=="" ? g:RecentWinmode : a:winmode)
    let bnum = bufnr(a:filename)
    " Note: includes unlisted and unloaded buffers (survives :bd for
    " example)
    let lasttabcmd = g:RecentWinmode!="newtab" && g:RecentReusetab
		\ ? "tabnext".s:lasttab : ""
    if bnum > 0
	" a:filename has a buffer (maybe not shown in any tab)
	let tabwin = s:GetTabWin(bnum)
	if tabwin[0] > 0
	    " it is shown in a tab, go there:
	    exec "tabnext" tabwin[0]
	    exec tabwin[1] "wincmd w"
	else
	    " goto buffer, new window needed; thiswin: reuse current window
	    " (should work, because recent_files is autowritten)
	    exec lasttabcmd
	    exec s:BufCmd[winmode] bnum
	endif
    elseif filereadable(a:filename)
	" :edit without a check edits new file (not wanted)
	" edit file, new window needed
	" maybe TODO: check if key is correct
	exec lasttabcmd
	exec s:EditCmd[winmode] a:filename
	if line("'\"") > 0 && line("'\"") <= line("$")
	    " :h last-position-jump
	    normal g'"
	endif
	let bnum = bufnr(a:filename)
    else
	" cannot read file, throw exception (a clean way to do that?)
	throw ":E447:"
    endif
    " w:RecentLastWrittenBuffer(Number)InThisWindow / Window last written
    let w:RecentWlw = bnum
endfunction

" edit recent_files, reuse buffer and tab+window if possible, split window
" if necessary
function s:VisitList()
    let s:lasttab = tabpagenr()
    let mod = &mod ? "mod" : "lis"
    " nofile check: if current buffer has no name and is not modified, use
    " its window:
    let nofile = !&mod && bufname("")==""
    let bnum = bufnr(s:RecentFullname)
    if bnum > 0
	" buffer for recent_files exists
	let tabwin = s:GetTabWin(bnum)
	if tabwin[0] > 0
	    " recent_files is shown in a tab, go there
	    exec "tabnext" tabwin[0]
	    exec tabwin[1] "wincmd w"
	else
	    " goto buffer, new window needed
	    exec (nofile ? "buf" : s:BufCmd[g:RecentWinmode.mod]) bnum
	endif
	if s:ListChanged
	    " if a file was written (-> recent_files changed with
	    " writefile(), Vim doesn't notice?), reload explicitly; silently
	    " discard manual changes
	    edit!
	    let s:ListChanged = 0
	endif
    else
	exec (nofile ? "edit" : s:EditCmd[g:RecentWinmode.mod]) s:RecentFullname
	" (do not always re-check if recent_files exists)
    endif
endfunction

" from recent_files buffer: open recent file under cursor
function <sid>Recentgf(winmode)
    " winmode: "", "thiswin", "newwin" or "newtab"
    noauto update	    " auto write recent_files
    " filename under cursor, trim whitespace (does this help?):
    let filename = substitute(getline("."), '^\s*\|\s*$','','g')
    " make filename full qualified:
    let filename = fnamemodify(filename, ":p")
    try
	call s:Visit(filename, a:winmode)
    catch /:E447:/
	" can't find file "..." in path
	let canwrite = filewritable(s:RecentFullname)
	if canwrite
	    del		" file under cursor, can be undone
	    noauto upd	" update: no-change possible if recent_files empty
	else
	    echo "Cannot write to" s:RecentFullname .", entry kept"
	endif
    catch /:E325:/
	" swap file found, do nothing
    endtry
endfunction

" BufWritePost *: add filename to recent_files
function s:RecentfilesAdd(filename)
    " filename: full qualified name
    " update recent_files (only) the first time the file in current window
    " is written
    let bnum = bufnr(a:filename)
    if exists("w:RecentWlw") && bnum == w:RecentWlw
	return
    endif
    " do not add files with suffixes to be ignored:
    if exists("b:RecentIgn")
	return
    endif
    let filename = fnamemodify(a:filename, ":~")
    let rfname = s:RecentFullname
    silent! let rflist = readfile(rfname)
    let li = index(rflist, filename)
    if li < 0
	" file not in list
	call insert(rflist, filename)
	call writefile(rflist, rfname)
	" next time, :edit! recent_files
	let s:ListChanged = 1
    endif
    let w:RecentWlw = bnum
endfunction

" manually add current file to recent_files
function s:AddFile()
    if bufname("")=="" || exists("b:RecentIgn")
	" do not open file list, if buffer has no filename or if filename is
	" to be ignored (e.g. recent_files)
	return
    endif
    " exact format of filenames used in recent_files
    let filename = fnamemodify(bufname(""), ":~")
    " open file list (!)
    Recent
    " add filename, only if not there
    if !search('\V'.escape(filename, '\'))
	call append(0, filename)
	1
    endif
    " ... always jump to entry
endfunction

" cycle through the Winmodes, affects :Recent and Enter
function s:CycleWinmodes()
    let wmi = (1+index(s:WinModes,g:RecentWinmode))%len(s:WinModes)
    let g:RecentWinmode = s:WinModes[wmi]
    echo "Winmode:" g:RecentWinmode
endfunction

function s:ToggleReusetab()
    let g:RecentReusetab = (g:RecentReusetab ? 0 : 1)
    echo "Reuse Tabpage:" g:RecentReusetab ? "on" : "off"
endfunction

" check recent_files before writing to it:
function s:Crfbwti()
    " Here: remove w:RecentWlw from all windows
    " Reason: this winvar avoids updating recent_files if user writes to a
    " file again (-> efficiency); after manually editing recent_files, we
    " don't know if that file is still in the list
    " Note: has no effect as long as recent_files is not written
    let [tnr, wnr, slz, &lz] = [tabpagenr(), winnr(), &lz, 1]
    noauto tabdo windo silent! unlet w:RecentWlw
    exec "norm!" tnr."gt".wnr."\<c-w>w"
    let &lz = slz
    " Note: when writing to recent_files, please do not fall asleep on the
    " write key ...
endfunction

command Recent call s:VisitList()
command RememberFile call s:AddFile()
command RecentWinmode call s:CycleWinmodes()
command RecentReusetab call s:ToggleReusetab()

let &cpo = s:save_cpo

" vim:set ts=8:
