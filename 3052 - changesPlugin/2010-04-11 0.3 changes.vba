" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/changesPlugin.vim	[[[1
37
" ChangesPlugin.vim - Using Signs for indicating changed lines
" ---------------------------------------------------------------
" Version:  0.3
" Authors:  Christian Brabandt <cb@256bit.org>
" Last Change: 2010/04/11
" Script:  http://www.vim.org/scripts/script.php?script_id=3052
" License: VIM License
" Documentation: see :help changesPlugin.txt
" GetLatestVimScripts: 3052 2 :AutoInstall: ChangesPlugin.vim


" ---------------------------------------------------------------------
"  Load Once: {{{1
if &cp || exists("g:loaded_changes")
 finish
endif
let g:loaded_changes       = 1
let s:keepcpo              = &cpo
set cpo&vim

let s:autocmd  = (exists("g:changes_autocmd")  ? g:changes_autocmd  : 0)
" ------------------------------------------------------------------------------
" Public Interface: {{{1
com! EnableChanges  call changes#GetDiff()|:call changes#Output()
com! DisableChanges call changes#CleanUp()

if s:autocmd
    call changes#Init()
endif
" =====================================================================
" Restoration And Modelines: {{{1
" vim: fdm=marker
let &cpo= s:keepcpo
unlet s:keepcpo

" Modeline
" vi:fdm=marker fdl=0
doc/changesPlugin.txt	[[[1
94
*changesPlugin.txt*  Print indication of changed lines for a buffer 

Author:  Christian Brabandt <cb@256bit.org>
Version: 0.3  Apr, 10th, 2010
Copyright: (c) 2010 by Christian Brabandt 	 *changesPlugin-copyright*
	   The VIM LICENSE applies to changesPlugin.txt (see |copyright|)
	   except use unicode instead of "Vim".  NO WARRANTY, EXPRESS OR
	   IMPLIED.  USE AT-YOUR-OWN-RISK.

==============================================================================
                                                              *changesPlugin*
1. Functionality

This plugin was written to help visualize which lines have been changes since
editing started for a file. The plugin was inspired by so called changed-bars,
available at other editors, such as Embarcadero C++ Builder (there it is
called Change Bars, see:
http://edn.embarcadero.com/article/33453#6PersonalDeveloperProductivity)
or Visual Studio where it is called indicator margin (see
http://blog.eveningcreek.com/?p=151).

ChangesPlugin.vim uses the |diff|-feature of vim and compares the actual
buffer with it's saved state. In order to highlight the indicator signs at the
first column, its using |signs|. For newly added lines, the first column will
be displayed with a leading '+' and highlighted using the DiffAdd highlighting
(see |hl-DiffAdd|), deleted lines will be indicated by a '-' with a
DiffDelete highlighting (see |hl-DiffDelete|) and modified lines will be
displayed using '*' and a DiffChange highlighting (see |hl-DiffChange|).

This means, that in order to use this plugin you need a vim, that was built
with |+signs|-support and |+diff|-support and you also need an executable diff
command. If neither of these conditions are met, changePlugin.vim will issue a
warning and abort.

							 *:EnableChanges*
By default the plugin is not enabled. To enable it enter >
    :EnableChanges
When you run this command, changesPlugin.vim diffs the current file agains
its saved file on disk and displays the changes in the first column.

							*:DisableChanges*
If you want to disable the plugin, enter >
    :DisableChanges

==============================================================================
							*changesPlugin-Config*
2. Configuring changesPlugin.vim

There are basically four different configuration options available.

2.1 Highlighte the whole line
By default, changesPlugin.vim will only indicate a change in the first column.
Setting g:changes_hl_lines to 1 will highlight the whole line. By default this
variable is unset (which is the same as setting it to 0).
If you'd like to have this, set this variable in your |.vimrc| >
    :let g:changes_hl_lines=1

2.2 Auto-refresh the changes
By default changesPlugin.vim will not automatically update the view. You can
however configure it to do so. This will use an |CursorHold| autocommand to
update the indicator signs after |'updatetime'| seconds in Normal mode when
no key is pressed. To enable this feature, put this in your |.vimrc| >
    let g:changes_autocmd=1

2.3 Show what the indicator signs mean.
By default, whenever you run |:EnableChanges|, changesVim will print a short
status message, what each sign means. If you don't want this, put this in your
|.vimrc| >
    :let g:changes_verbose=0

2.4 Specify different colors.
changesVim uses the highlighting used for |diff| mode to indicate the change
in a buffer. This is consistent, since when you're already used to |vimdiff|
you'll probably also know the highlighting. If for any reason you do not like
the colors, you have to define your own highlighting items.
If for example you want the DiffAdd highlighting to be displayed like White on
a Blue background, you can define it as follows in your |.vimrc| >

    :hi DiffAdd term=bold ctermbg=4 guibg=DarkBlue

In the same way, you can change DiffDelete for indicating deleted lines and
DiffChange for indicating modified lines. You can also specify your favorite
highlighting colors using your own build |colorscheme|.
==============================================================================
3. changesPlugin History				*changesPlugin-history*
    0.3: Apr 11, 2010:  BF: redraw, so that the diff window will not be
			displayed
			NF: enabled GLVS (see |GLVS|)
    0.2: Apr 11, 2010:	Added Documentation
			created an autoload version
    0.1: Apr 10, 2010:	First working version

==============================================================================
vim:tw=78:ts=8:ft=help
autoload/changes.vim	[[[1
207
" Changes.vim - Using Signs for indicating changed lines
" ---------------------------------------------------------------
" Version:  0.3
" Authors:  Christian Brabandt <cb@256bit.org>
" Last Change: 2010/04/11
" Script:  http://www.vim.org/scripts/script.php?script_id=3052
" License: VIM License
" Documentation: see :help changesPlugin.txt
" GetLatestVimScripts: 3052 2 :AutoInstall: ChangesPlugin.vim

" Documentation:"{{{1
" To see differences with your file, exexute:
" :EnableChanges
"
" The following variables will be accepted:
"
" g:changes_hl_lines
" If set, all lines will be highlighted, else
" only an indication will be displayed on the first column
" (default: 0)
"
" g:changes_autocmd
" Updates the indication for changed lines automatically,
" if the user does not press a key for 'updatetime' seconds when
" Vim is not in insert mode. See :h 'updatetime'
" (default: 0)
"
" g:changes_verbose
" Output a short description, what these colors mean
" (default: 1)
"
" Colors for indicating the changes
" By default changes.vim displays deleted lines using the hilighting
" DiffDelete, added lines using DiffAdd and modified lines using
" DiffChange.
" You can see how these are defined, by issuing 
" :hi DiffAdd
" :hi DiffDelete
" :hi DiffChange
" See also the help :h hl-DiffAdd :h hl-DiffChange and :h hl-DiffDelete
"
" If you'd like to change these colors, simply change these hilighting items
" see :h :hi

" Check preconditions"{{{1
fu changes#Check()
    if !has("diff") 
	call changes#WarningMsg("Diff support not available in your Vim version.")
	call changes#WarningMsg("changes plugin will not be working!")
	finish
    endif

    if  !has("signs")
	call changes#WarningMsg("Sign Support support not available in your Vim version.")
	call changes#WarningMsg("changes plugin will not be working!")
	finish
    endif

    if !executable("diff") || executable("diff") == -1
	call changes#WarningMsg("No diff executable found")
	call changes#WarningMsg("changes plugin will not be working!")
	finish
    endif
endfu

fu! changes#WarningMsg(msg)"{{{1
    echohl WarningMsg
    echo a:msg
    echohl Normal
endfu

fu! changes#Output()"{{{1
    if s:verbose
	echohl Title
	echo "Differences will be highlighted like this:"
	echohl Normal
	echo "========================================="
	echohl DiffAdd
	echo "+ Added Lines"
	echohl DiffDelete
	echo "- Deleted Lines"
	echohl DiffChange
	echo "* Changed Lines"
	echohl Normal
    endif
endfu

fu! changes#Init()"{{{1
    let s:hl_lines = (exists("g:changes_hl_lines") ? g:changes_hl_lines : 0)
    let s:autocmd  = (exists("g:changes_autocmd")  ? g:changes_autocmd  : 0)
    let s:verbose  = (exists("g:changes_verbose")  ? g:changes_verbose  : 1)

    let s:signs={}
    let s:ids={}
    let s:signs["add"] = "texthl=DiffAdd text=+ texthl=DiffAdd " . ( (s:hl_lines) ? " linehl=DiffAdd" : "")
    let s:signs["del"] = "texthl=DiffDelete text=- texthl=DiffDelete " . ( (s:hl_lines) ? " linehl=DiffDelete" : "")
    let s:signs["chg"] = "texthl=DiffChange text=* texthl=DiffChange " . ( (s:hl_lines) ? " linehl=DiffDelete" : "")

    let s:ids["add"]   = hlID("DiffAdd")
    let s:ids["del"]   = hlID("DiffDelete")
    let s:ids["ch"]    = hlID("DiffChange")
    call changes#DefineSigns()
    call changes#AuCmd(s:autocmd)
    call changes#Check()
endfu

fu! changes#AuCmd(arg)"{{{1
    if s:autocmd && a:arg
	augroup Changes
		autocmd!
		au CursorHold * :call changes#GetDiff()
	augroup END
    else
	augroup Changes
		autocmd!
	augroup END
    endif
endfu

fu! changes#DefineSigns()"{{{1
    exe "sign define add" s:signs["add"]
    exe "sign define del" s:signs["del"]
    exe "sign define ch"  s:signs["chg"]
endfu

fu! changes#GetDiff()"{{{1
    call changes#Init()
    let o_lz=&lz
    let o_fdm=&fdm
    setl lz
    sign unplace *
    call changes#MakeDiff()
    let b:diffhl={'add': [], 'del': [], 'ch': []}
    let line=1
    while line <= line('$')
	let id=diff_hlID(line,1)
	if  (id == 0)
	    let line+=1
	    continue
	elseif (id == s:ids["add"])
	    let b:diffhl['add'] = b:diffhl['add'] + [ line ]
	else
	    let b:diffhl['ch']  = b:diffhl['ch'] + [ line ]
	endif
	let line+=1
    endw
    " Switch to other buffer and check for deleted lines
    wincmd p
    " For some reason, getbufvar setbufvar do not work, so 
    " we use a temporary script variable here
    let s:temp={'del': []}
    let line=1
    while line <= line('$')
	let id=diff_hlID(line,1)
	if (id == s:ids["add"])
	    let s:temp['del'] = s:temp['del'] + [ line ]
	endif
	let line+=1
    endw
    wincmd p
    let b:diffhl['del'] = s:temp['del']
    call changes#PlaceSigns(b:diffhl)
    call changes#DiffOff()
    redraw
    let &lz=o_lz
    let &fdm=o_fdm
endfu

fu! changes#PlaceSigns(dict)"{{{1
    for [ id, lines ] in items(a:dict)
	for item in lines
	    exe "sign place " . item . " line=" . item . " name=" . id . " buffer=" . bufnr('')
	endfor
    endfor
endfu
	    
fu! changes#MakeDiff()"{{{1
    " Get diff for current buffer with original
    vert new
    set bt=nofile
    r #
    0d_
    diffthis
    wincmd p
    diffthis
endfu

fu! changes#DiffOff()"{{{1
    " Turn off Diff Mode and close buffer
    wincmd p
    diffoff!
    q
endfu

fu! changes#CleanUp()"{{{1
    sign unplace *
    for key in s:keys
	exe "sign undefine " key
    endfor
"    sign undefine del
"    sign undefine ch
    call changes#AuCmd(0)
endfu


" Modeline "{{{1
" vi:fdm=marker fdl=0
