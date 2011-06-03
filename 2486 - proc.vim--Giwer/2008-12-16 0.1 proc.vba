" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/proc.vim	[[[1
300
" proc.vim 
" A frontend for 'ps' and 'kill'
" Last Change:	Di 16.Dez 2008
" Maintainer:	Johann Giwer johanngiwer@web.de
" License:	This file is placed in the public domain.
" Version:      0.1
"
" Functions:
"
" Ps()		show a list of processes in a new buffer.
"
" Mappings:
"
" <leader>k 	or <Plug>ProcKill
" 		kill marked processes or the one in the current row,
" 		if none is marked. You will be asked for a signal,
" 		either as number or name (i.e. 'HUP'). <Esc> cancels
" 		killing and removes marks.
"
" <leader><leader>	or <Plug>ProcMark
" 		toggle mark for killing.
"
" <leader><Esc>	or <Plug>ProcUnMark
" 		remove all marks.
"
" <leader>s		or <Plug>ProcSortByCurrentColumn
" 		sort processes by current column.
"
" <leader>n		or <Plug>ProcUnSort
" 		restore unsorted view.
"
" <leader>a		or <Plug>ProcShowAllProcesses
" 		show all processes. 
"
" <leader>u		or <Plug>ProcShowUserProcesses
" 		show only user processes (default).
"
" <leader>c		or <Plug>ProcSortByCpu
" 		sort processes by percentage of cpu.
"
" <leader>m		or <Plug>ProcSortByMem
" 		sort processes by percentage of mem.
"
" <leader>p		or <Plug>ProcUpdate
" 		update the process buffer, keep sorting order.
"
" <leader>q		or <Plug>ProcQuit
" 		close the process buffer.
"
" Marking and killing also works in visual line mode, which is usefull for
" killing a bunch of processes.
"
" Settings:
"
" g:PsArgs	
" 		a string containing format options for 'ps'
" 		Default: "ux"
"
" 		Example: 
" 		let g:PsArgs = "x -o user,pid,s,tty,pcpu,pmem,time,args"
"
" 		If you define a format list via '-o', it must contain
" 		a field 'pid' to keep this script working. See also ps (1).
"
" g:PsAutoUpdate
" 		If you are using gvim and switch to another
" 		buffer|window, the process view will be updated as
" 		soon as the focus reenters its window. If you don't
" 		like this behaviour, set g:PsAutoUpdate to 0
"


if !exists("g:PsArgs")
    let g:PsArgs = "ux"
endif

if !exists("g:PsAutoUpdate")
    let g:PsAutoUpdate=1
endif

function! Ps()
    silent! edit proc

    let s:Args = g:PsArgs
    let s:SortedBy = ""
    let s:LastUpdate = ""
    let s:Marked = []

    setlocal buftype=nofile
    setlocal noswapfile
    setlocal nowrap
    setlocal cursorline


    if g:PsAutoUpdate 
        augroup proc
            au!
            au BufEnter    <buffer> :call s:Update()
            au FocusGained <buffer> :call s:Update()
        augroup END
    endif

    syntax match Keyword  / [RD][sl+] /
    syntax match Comment  /^[A-Z% ]\+$/
    syntax match Function / [1-9]\d\.\d /
    syntax match Marked   /$^/
    highlight link Marked WarningMsg

    if !hasmapto('<Plug>ProcQuit')
        map <buffer><leader>q 				<Plug>ProcQuit
    endif
    if !hasmapto('<Plug>ProcSortByCurrentColumn')
        map <buffer><leader>s 				<Plug>ProcSortByCurrentColumn
    endif
    if !hasmapto('<Plug>ProcSortByMem')
        map <buffer><leader>m 				<Plug>ProcSortByMem
    endif
    if !hasmapto('<Plug>ProcSortByCpu')
        map <buffer><leader>c 				<Plug>ProcSortByCpu
    endif
    if !hasmapto('<Plug>ProcUnSort')
        map <buffer><leader>n 				<Plug>ProcUnSort
    endif
    if !hasmapto('<Plug>ProcUnMark')
        map <buffer><leader><Esc>			<Plug>ProcUnMark
    endif
    if !hasmapto('<Plug>ProcMark')
        map <buffer><leader><leader>		<Plug>ProcMark
    endif
    if !hasmapto('<Plug>ProcKill')
        map <buffer><leader>k 				<Plug>ProcKill
    endif
    if !hasmapto('<Plug>ProcUpdate')
        map <buffer><leader>p 				<Plug>ProcUpdate
    endif
    if !hasmapto('<Plug>ProcShowAllProcesses')
        map <buffer><leader>a 				<Plug>ProcShowAllProcesses
    endif
    if !hasmapto('<Plug>ProcShowUserProcesses')
        nmap <buffer><leader>u 				<Plug>ProcShowUserProcesses
    endif

    noremap <buffer><Plug>ProcQuit 					:bd <CR>
    noremap <buffer><Plug>ProcSortByCurrentColumn	:call <SID>SortByCurrentColumn () <CR>
    noremap <buffer><Plug>ProcSortByMem 			:call <SID>SortByField ("%MEM") <CR>
    noremap <buffer><Plug>ProcSortByCpu 			:call <SID>SortByField ("%CPU") <CR>
    noremap <buffer><Plug>ProcUnSort 				:let <SID>SortedBy="" <CR> :call <SID>Update () <CR>
    noremap <buffer><Plug>ProcUnMark				:let <SID>Marked=[] <CR> :syntax clear Marked <CR>
    noremap <buffer><Plug>ProcMark 					:call <SID>ToggleMarked () <CR>j
    noremap <buffer><Plug>ProcKill 					:call <SID>Kill ("15") <CR>
    noremap <buffer><Plug>ProcUpdate 				:call <SID>Update () <CR>
    noremap <buffer><Plug>ProcAllProcs 				:call <SID>ShowAllProcesses () <CR>
    noremap <buffer><Plug>ProcUserProcs 			:call <SID>ShowUserProcesses () <CR>


    if exists("s:proc_loaded")
        call s:Update ()
        return
    endif
    let s:proc_loaded = 1

    function! s:SortByField ( field )
        let l:index = index(s:columnHeaders,a:field) 
        if l:index > -1
            call s:SortByColumn ( l:index )
        endif
    endfunction 


    " Sort process view by column @nr
    function! s:SortByColumn ( nr )
        let l:field = s:columnHeaders[ a:nr ]
        let l:view = winsaveview()
        if count(["USER","COMMAND","ARGS","TTY","STAT","S","TT"],l:field)
            let s:SortedBy = l:field
            exe printf(':2,$sort /\([^ ]\+ \+\)\{%d\}/', a:nr )
        else
            let s:SortedBy = l:field
            exe printf(':2,$sort n /\([^ ]\+ \+\)\{%d\}\d\+[:.]\d\+[:.]/', a:nr )
            exe printf(':2,$sort n /\([^ ]\+ \+\)\{%d\}\d\+[:.]/', a:nr )
            exe printf(':2,$sort n /\([^ ]\+ \+\)\{%d\}/', a:nr )
        endif
        call winrestview(l:view)
        echo s:Status()
    endfunction 

    function! s:ShowUserProcesses()
        let s:Args = g:PsArgs
        call s:Update()
        normal 1000zb
    endfunction

    function! s:SortByCurrentColumn ()
        let l:column  = len( split ( getline('.')[0:col('.')]))
        let l:column = min([ l:column, len( s:columnHeaders ) ])
        call s:SortByColumn( l:column - 1 )
    endfunction

    function! s:ShowAllProcesses() 
        let s:Args = "a" . g:PsArgs
        call s:Update()
        normal 1000zb
    endfunction

    function s:ToggleMarked () range
        for pid in s:GetPID(a:firstline,a:lastline)
            let l:idx = index( s:Marked, pid )
            if l:idx == -1
                echo pid
                call insert (s:Marked, pid)
            else
                call remove (s:Marked, l:idx)
            endif
        endfor
        let l:pos = index( s:columnHeaders, 'PID' )
        syntax clear Marked       
        if len( s:Marked )
            exe 'syntax match Marked /\([^ ]\+ \+\)\{'. l:pos . '\}\(' . join(s:Marked, '\|') . '\) .\+/'
        endif
    endfunction

    function! s:GetPID (lnum1,lnum2) 
        let l:pos = index(s:columnHeaders, "PID")
        let l:pids = []
        for i in range(a:lnum1,a:lnum2)
            let l:pids += [split(getline(i))[l:pos]]
        endfor
        return l:pids  
    endfunction

    function! s:Kill (signal) abort range
        if line ('.') == 1
            return
        endif

        if len( s:Marked ) == 0
            let s:Marked =   s:GetPID(a:firstline,a:lastline)  
        endif
        let l:pids = join( s:Marked )
        
        let signal = input("Kill " . l:pids ." with signal: ",a:signal)
        let status = system( printf("kill -%d %s\n" ,signal, l:pids ))
        
        if len( status ) != 0
            echohl WarningMsg 
            echon status
            echohl None
            2sleep
            call s:Update()
        elseif len( signal )
            sleep
            call s:Update()
            for pid in s:GetPID(2, line('$'))
                if index( s:Marked,  pid) != -1
                    echohl WarningMsg 
                    echon " (Warning: not all processes where properly removed)"
                    echohl None
                    break
                endif
            endfor
        endif
        let s:Marked = [] 
        syntax clear Marked       
    endfunction

    function s:Status()
        if s:Args == g:PsArgs
            let l:statusline = " user processes"
        else
            let l:statusline = " all processes"
        endif
        if len( s:LastUpdate )
            let l:statusline .= " at " . s:LastUpdate 
        endif
        if len ( s:SortedBy )
            let l:statusline .= " (sorted by " .  s:SortedBy  . ")"
        endif
        return l:statusline
    endfunction

    function! s:Update () 
        let l:view = winsaveview()
        "let s:SortedBy = ""
        if exists("*strftime")
            let s:LastUpdate = strftime ("%X")
        endif
        silent! exe ":1,$!ps -ww " . s:Args . " | grep -v 'ps -ww' "
        let s:columnHeaders = split(getline(1))
        if s:SortedBy != ""
            call s:SortByField( s:SortedBy )
        else
            echo s:Status()
        endif
        call winrestview(l:view)
    endfunction

    call s:Update ()
endfunction

vim:tw=78:ts=8:noet:ft=vim:ff=unix:
doc/proc.txt	[[[1
78
*proc.txt* 	A frontend for 'ps' and 'kill'

Functions:

Ps()									*Ps()*	
	        show a list of processes in a new buffer.
	        Use the keys described below to select and kill processes.

Mappings:

<leader>k 	
<Plug>ProcKill
	        kill marked processes or the one in the current row, if none is
	        marked. You will be asked for a signal, either as number or name
	        (i.e. 'HUP'). <Esc> cancels killing and removes marks.

<leader><leader>
<Plug>ProcMark
		toggle mark for killing.

<leader><Esc>
<Plug>ProcUnMark
		remove all marks.

<leader>s	
<Plug>ProcSortByCurrentColumn
		sort processes by current column.

<leader>n	
<Plug>ProcUnSort
		restore unsorted view.

<leader>a	
<Plug>ProcShowAllProcesses
		show all processes. 

<leader>u	
<Plug>ProcShowUserProcesses
	        show only user processes (default).

<leader>c	
<Plug>ProcSortByCpu
		sort processes by percentage of cpu usage.

<leader>m	
<Plug>ProcSortByMem
		sort processes by percentage of memory usage.

<leader>p	
<Plug>ProcUpdate
		update the process buffer, keep sorting order.

<leader>q	
<Plug>ProcQuit
		close the process buffer.

Marking and killing also works in visual line mode, which is usefull for
killing a bunch of processes.

Settings:

g:PsArgs	
		a string containing format options for 'ps'
		Default: "ux"

		Example: 
		let g:PsArgs = "x -o user,pid,s,tty,pcpu,pmem,time,args"

If you define a format list via '-o', it must contain a field 'pid'
to keep this script working. See also ps (1).

g:PsAutoUpdate
	        If you are using gvim and switch to another buffer|window, the
	        process view will be updates as soon as the focus reenters its
	        window. If you don't like this behaviour, set this flag to 0


vim:tw=78:ts=8:noet:ft=help:
