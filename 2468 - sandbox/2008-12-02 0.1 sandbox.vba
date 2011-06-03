" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/sandbox.vim	[[[1
519
" sandbox.vim: Vim script for managing subversion sandbox
" Author: Wenzhi Liang <wenzhi.liang _at_ gmail.com>
"
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
"
" Install:
" This plugin will be distributed as a vimball. So simply edit the vimbal and
" :so it.
"
" Usage:
" ':Sandbox <sandbox_directory>' on the command line will create a new buffer
" which list the currnet status of the sandbox. In the buffer, a few handy
" mapping are available like diffing, commit, revert, etc. See the help at 
" the bottom of the buffer.
"
" Configuration:
" Define an array called g:sandbox_prefered_gui_diff as a sequence of prefered gui diffing tool, in that order.
" E.G. in your vimrc file, 'let g:sandbox_prefered_gui_diff=['meld','tkdiff'] will cause this script to search
" and use meld, in the absence of which, tkdiff.
"
" BUG:
" - If a file has local modification and and 'update' cause it to be modified or conflict, the status is wrong.
"
" TODO: 
" - Is it possible to get rid of the "no quotation mark" restriction on commit message?
" - Add functionality to execute arbitary command on a line. E.g. unlock
" - Add functionality to add stuff to repo
" - Add command to hide all files that needs update.
" - There must be a way to make the command more generic. There are a lot of duplicated code
" - Mass command result in a buffer update. Is there a clever way of removing this dependency?
"
"
" ChangLog: {{{
" Tue Dec  2  Added mapping for arbitary svn command execution.
" Thu Nov 27  Renamed to sandbox.vim
" Wed Nov 26  Fixed issue with clean snapshot
" Tue Nov 18  Allow user to specify prefered gui diffing tool. 
"             Changed default map. No confusing capitalized mapping anymore, except for U.
" Fri Nov 14  Fixed update. Change mapping <C-R> to <F5> because it clash with things like C-RC-f
" Thu Nov 13  svn output sorted.
" Wed Nov 12  Sandbox path printed at first line. Restricted command to be within actual svn output
" Tue Nov 11  Overhaul or mass command. Mapping redefined.
" Wed Sep 24  Changed implementation of GetFileName() so that it blindly looks for string[21:]
" Wed Sep 10  Added <C-R> for refreshing the buffer
" Wed Aug 20  Added support for 'A'. Added Edit command
" Sat Jul 19  Fully working version.
" Thu Jul 17  Removing reverted or commited line works.
" Tue Jul 15  First working version without range support and hardcoded
"             startup directory.
"           }}}
" """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"if exists("sandbox_loaded")
"    finish
"else
"    let sandbox_loaded=1
"endif

if !exists("g:sandbox_prefered_gui_diff")
    let g:sandbox_prefered_gui_diff=['tkdiff', 'meld']
endif

" {{{ Local vars
let s:sandbox_buffer_name = '__sandbox__'
let s:root_dir=""
let s:selected_files=[]
let s:svn_msg=""
let s:first_line = 0
let s:last_line = 0
let s:supported_gui_diff=['meld', 'tkdiff']
let s:gui_diff_cmd=""

let s:commit_prompt = "Commit message, no quotation marks (leave blank to cancel): "
let s:revertall_prompt = "Are you sure you want to revert the hilighted files? " 
let s:anycommand_prompt = "Input the svn command you want to run (without the svn part): "
" }}}

" {{{ Check requirment, doesn't return
for d in g:sandbox_prefered_gui_diff
    if index( s:supported_gui_diff, d) < 0
        continue
    endif
    if executable( d )
        "echo "Found gui diffing tool: " . d
        let s:gui_diff_cmd=d
        break
    endif
endfor

if s:gui_diff_cmd == ""
    echoerr "Dependency error: missing gui diffing tool."
    finish
endif

if !executable('svn')
    echoerr "Dependency error: missing svn."
    finish
endif

if version < 700
    echoerr "Dependency error: Vim version older than 7.0."
    finish
endif
"}}}

"Check if the current line is indeed part of the "svn st" log
function! <SID>__IsStatusLine(l)
    if s:first_line == 0
        return 0
    endif

    if a:l < s:first_line || a:l > s:last_line
        return 0
    else
        return 1
    endif
endfunc


"Prompt the user, return a list, where the first is boolean to signal if
"following action should be taken, the second is a string 
function! <SID>__Prompt(p)
    let ans = input(a:p)
    if ans != ""
        let s:svn_msg=ans
        return 1
    else
        let s:svn_msg=""
        return 0
    endif
endfunc

"Tag (select) a file for command over multiple files
function! <SID>TagLine()
    exe 'lcd ' . s:root_dir
    let num=line('.')
    if ! <SID>__IsStatusLine(num)
        return
    endif
    setlocal modifiable
    let l = getline('.')
    let idx = -1
    let fn = ""
    if l =~ ' (+)$'
        .s/ (+)//g
        let l = getline('.')
        let fn = <SID>GetFileName(l)
        let idx = index( s:selected_files, fn )
    else
        let fn = <SID>GetFileName(l)
        exec "normal  A (+)"
    endif

    if idx != -1
        call remove( s:selected_files, idx )
        "echo s:selected_files
    else
        call add(s:selected_files, fn)
        "echo s:selected_files
    endif
    setlocal nomodifiable
    normal j
endfunc

"Reset Selection
function! <SID>UnselectAll()
    let s:selected_files=[]

    setlocal modifiable
    %s, (+)$,,g
    setlocal nomodifiable
endfunc

"Get a file name from a line, returns 'XXX' when the line isn't a svn log
function! <SID>GetFileName(l)
    exe 'lcd ' . s:root_dir
    let r = substitute( a:l, '^.\{20}','','g')
    if filereadable(r) || isdirectory(r)
        return r
    else
        return "XXX"
    endif
endfunc

"Run svn command on a single file or a list of files
function! <SID>__ExeSvnCommand(cmd, list, silent)
    exe 'lcd ' . s:root_dir
    if type(a:list) == 3 "actually a list
        let cli = "svn " . a:cmd . ' ' . join(a:list, ' ')
    else "Assuming it is a string
        let cli = "svn " . ' ' . a:cmd . ' "' . a:list . '"'
    endif
    if a:silent
        silent exec '!' . cli
    else
        exec '!' . cli
    endif
    "echo cli
endfunc

"Run svn command on a single line
function! <SID>__ExeSvnMassCommand(cmd)

    call <SID>__ExeSvnCommand( a:cmd, s:selected_files, 1 )
    call <SID>UpdateBuffer(0)
endfunc


"Remove the current line when it is not needed anymore, dd will not work because
"'d' is mapped to sth else.
function! <SID>__RemoveCurrentLine()
    let save_reg=@"
    setlocal modifiable
    normal 0DgJ
    setlocal nomodifiable
    let @"=save_reg
endfunc


"Do revert on the current log line
function! <SID>Revert()
    let num=line('.')
    if ! <SID>__IsStatusLine(num)
        return
    endif
    let l = getline('.')
    let fn = <SID>GetFileName(l)
    if fn != "XXX"
        let ans = input("Are you sure you want to revert changes made in " . fn . '? ') 
        if toupper(ans) == 'Y' 
            call <SID>__ExeSvnCommand( 'revert', fn, 1 )
            call <SID>__RemoveCurrentLine()
        endif
    endif
endfunc

"Commit single file on current line
function! <SID>Commit()
    let num=line('.')
    if ! <SID>__IsStatusLine(num)
        return
    endif
    let l = getline('.')
    let fn = <SID>GetFileName(l)
    if fn != "XXX"
        let ans = input("Commit message, no quotation marks (leave blank to cancel): ") 
        if ans != ""
            call <SID>__ExeSvnCommand( 'commit -m ' . '"' . ans . '"', fn, 1 )
            call <SID>__RemoveCurrentLine()
        endif
    endif
endfunc

"Resolved single file
function! <SID>Resolved()
    let num=line('.')
    if ! <SID>__IsStatusLine( num )
        return
    endif
    let l = getline('.')
    if  l !~ '^C'
        return
    endif
    let fn = <SID>GetFileName(l)
    if fn != "XXX"
        call <SID>__ExeSvnCommand( 'resolved ',  fn, 1 )
        setlocal modifiable
        :.s/^C/M/g
        setlocal nomodifiable
    endif
endfunc

"Revert on a range of lines...
function! <SID>RevertAll()
    if len( s:selected_files ) == 0
        call <SID>Revert()
        return
    endif
    let ans = input(s:revertall_prompt)
    if toupper(ans) != 'Y' 
        return
    endif
    call <SID>__ExeSvnMassCommand('revert')
endfunc

"Commit on a range of lines
function! <SID>CommitAll()
    if len( s:selected_files ) == 0
        call <SID>Commit()
        return
    endif
    let cont=<SID>__Prompt( s:commit_prompt )
    if cont 
        call <SID>__ExeSvnMassCommand('commit -m ' . '"' . s:svn_msg . '"')
    else
        echo "Operation canceled."
    endif
endfunc


"Resolved single file
function! <SID>ResolvedAll()
    if len( s:selected_files ) == 0
        call <SID>Resolved()
        return
    endif
    call <SID>__ExeSvnMassCommand('resolved')
endfunc

"Update a line
function! <SID>Update()
    let num=line('.')
    if ! <SID>__IsStatusLine(num)
        return
    endif
    let l = getline('.')
    if  l !~ '^.      \*'
        "echo "Nothing to be done"
        return
    endif
    let fn = <SID>GetFileName(l)
    if fn != "XXX"
        call <SID>__ExeSvnCommand( "update --accept 'postpone'",  fn, 1 )
        setlocal modifiable
        if l =~ '^C'
            :.s/^.      \*/C       /g
        else
            :.s/^.      \*/U       /g
        endif
        setlocal nomodifiable
    endif
endfunc

function! <SID>UpdateAll()
    if s:first_line == 0
        return
    endif
    exec s:first_line . ',' s:last_line . "call <SID>Update()"
endfunc

"Diff single file on current line
function! <SID>Diff()
    let num=line('.')
    if ! <SID>__IsStatusLine(num)
        return
    endif
    "echo "Entering " . s:root_dir
    exe 'lcd ' . s:root_dir
    let l = getline('.')
    let fn = <SID>GetFileName(l)
    if fn != "XXX"
        silent exe '!' . s:gui_diff_cmd . ' ' . fn . '&'
    endif
endfunc

"We don't support diffing multiple files.... yet
function! <SID>ErrDiffRange() range
    echoerr "Diff operation is not available on range" 
endfunc

" Load the file into a new tab
function! <SID>Edit()
    let num=line('.')
    if ! <SID>__IsStatusLine(num)
        return
    endif
    exe 'lcd ' . s:root_dir
    let l = getline('.')
    let fn = <SID>GetFileName(l)
    if fn != "XXX"
        silent exe "tabe " . fn
    endif
endfunc

" Run arbitary command from the user
function! <SID>AnyCommand()
    if len( s:selected_files ) != 0
        echo "Undo all selections. This command is currently only available for a single file."
        return
    endif
    let num=line('.')
    if ! <SID>__IsStatusLine(num)
        return
    endif
    exe 'lcd ' . s:root_dir
    let l = getline('.')
    let fn = <SID>GetFileName(l)
    if fn != "XXX"
        let cont=<SID>__Prompt( s:commit_prompt )
        if cont 
            call <SID>__ExeSvnCommand( s:svn_msg, fn, 0 )
        endif
    endif
endfunc

function! <SID>PrintHelp()
    "Should only be called within CreateBuffer()
    setlocal modifiable
    normal Go
    call setline('.', " ---------- H E L P -----------" )
    normal o
    call setline('.', " c       Commit a single file or selected files.")
    normal o
    call setline('.', " d       Diff the file specified in the current line.")
    normal o
    call setline('.', " e       Open the file in question in a new tab.")
    normal o
    call setline('.', " r       Revert a single file or selected files." )
    normal o
    call setline('.', " t       Select/Unselect a single file." )
    normal o
    call setline('.', " u, U    Update a single file or update all files." )
    normal o
    call setline('.', " v       Resolved a single file or selected files.")
    normal o
    call setline('.', " C-D     Reset selection.")
    normal o
    call setline('.', " <F5>    Refresh the buffer.")
    normal o
    call setline('.', " q       Quit")
    setlocal nomodifiable
endfunc

 
function! <SID>UpdateBuffer(first)
    if !a:first
        echo "Refreshing svn result....."
    endif
    exe "lcd  " . s:root_dir
    mapclear <buffer>
    setlocal modifiable
    normal ggdG
    "Somehow this command will leave an empty first line in the buffer
    :r!svn status . --quiet --show-updates --non-interactive
    let l = getline(2)
    if l =~ '^Status against' 
        "No local change"
        let s:first_line=0
        let s:last_line=0
    else
        let s:first_line=2
        let s:last_line=line('.') - 1 " Last line is Status ... and an empty line 
        exe s:first_line . ',' s:last_line . 'sort'
    endif
    normal gg
    call setline('.', 'Current sandbox: ' . s:root_dir)
    call <SID>PrintHelp()
    normal ggj
    call <SID>__SetupMapping()
    setlocal nomodifiable

    "Reset the selection
    let s:selected_files=[]
endfunc

function! <SID>__SetupMapping()
    nnoremap <buffer> <silent> r :call <SID>RevertAll()<CR>
    "nnoremap <buffer> <silent> R :call <SID>RevertAll()<CR>
    nnoremap <buffer> <silent> d :call <SID>Diff()<CR>
    vnoremap <buffer> <silent> d :call <SID>ErrDiffRange()<CR>
    nnoremap <buffer> <silent> c :call <SID>CommitAll()<CR>
    "nnoremap <buffer> <silent> C :call <SID>CommitAll()<CR>
    nnoremap <buffer> <silent> t :call <SID>TagLine()<CR>
    nnoremap <buffer> <silent> u :call <SID>Update()<CR>
    nnoremap <buffer> <silent> U :call <SID>UpdateAll()<CR>
    nnoremap <buffer> <silent> v :call <SID>ResolvedAll()<CR>
    nnoremap <buffer> <silent> x :call <SID>AnyCommand()<CR>
    "nnoremap <buffer> <silent> V :call <SID>ResolvedAll()<CR>
    nnoremap <buffer> <silent> e :call <SID>Edit()<CR>
    nnoremap <buffer> <silent> <F5> :call <SID>UpdateBuffer(0)<CR>
    nnoremap <buffer> <silent> <C-D> :call <SID>UnselectAll()<CR>
    nnoremap <buffer> <silent> q :bwipeout!<CR>
endfunc

"Initialise the buffer and set mapping
function! <SID>CreateBuffer(p)
    if !isdirectory(a:p)
        echoerr "Invalid directory name."
        return
    endif
    echo "Getting list of changes for " . a:p . "..."
    let s:root_dir = a:p
    exec "silent 18split " . s:sandbox_buffer_name
    "exe "lcd  " . a:p
    " TODO: The following command should be an more elegant solution than
    " entering directory each time a function is called. However, it doesn't
    " work....
    "exec "au BufEnter <buffer> lcd " . s:root_dir

    setlocal buftype=nofile
    "47 is '/'
    setlocal isk+=47-57,_,a-z,A-Z
    syn clear
    syn match SvnDiffModified "^M\>"
    syn match SvnDiffConflict "^C\>"
    syn match SvnDiffSelected " (+)$"
    syn match SvnDiffRevision "\<\d\+\>"
    hi link SvnDiffModified Identifier
    hi link SvnDiffConflict Error
    hi link SvnDiffSelected Keyword
    hi link SvnDiffRevision Number
    call <SID>UpdateBuffer(1)
endfunc

"""""""""""""""""""""""""""""""""
" MAIN
"""""""""""""""""""""""""""""""""
if !exists(":Sandbox")
    command! -nargs=1 -complete=dir Sandbox call <SID>CreateBuffer(<q-args>)
endif

finish


Files:
plugin/sandbox.vim
" EoF vim:ts=4:sw=4:et:
