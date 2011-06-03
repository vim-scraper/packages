" File: RemoteSaveAll.vim
" Version: 1.0 2004-04-12
" Description: Command :RWALL (synonym - :WALL) signals all running vim 
"              instnces to save their modified files. "All vim instances" means 
"              all clientserver-enabled vims. Local vim is also saved.
"              With bang added, :RWALL! sends ":wall!" command.
"              Without bang, :RWALL sends ":wall" command.
" Maintainer: Yakov Lerner <jilerner@yahoo.com>

function! RemoteSaveAll(bang)
    if a:bang=='!'
	let cmd='wall! "from remote'
    else
	let cmd='wall "from remote'
    endif

    let srvlist=serverlist()
    let num_remotes=0
    while srvlist != ""
	let srv=substitute(srvlist, "\n.*$", '', "")
	let srvlist=substitute(srvlist, "[^\n]*\n", '', "")
	if srv != v:servername
	    " call remote_send( srv, "\<C-\>\<C-N>:wall\n")
	    call remote_send( srv, "\<C-\>\<C-N>:".cmd."\n")
	    let num_remotes=num_remotes+1
	endif
    endw

    " we also save local vim
    norm cmd

    echo "Saved".a:bang." 1 local and ".num_remotes." remote vim(s) "
    return num_remotes
endfunction

:command! -bang WALL call RemoteSaveAll(<q-bang>)
:command! -bang RWALL call RemoteSaveAll(<q-bang>)
