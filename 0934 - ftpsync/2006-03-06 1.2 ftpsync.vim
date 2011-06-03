"
"	File:    ftpsync.vim
"	Author:  Fabien Bouleau (syrion AT tiscali DOT fr)
"	Version: 1.2
"
"	Last Modified: March 06th, 2006
"
"	Usage:
"
"   The FtpUpdate script provides you the ability to synchronize your files 
"   with a server. The main idea when I created this script was to use GVim 
"   and CVS to edit the files on my local PC rather than on the remote server 
"   which might be sometimes really slow. 
"   
"   With S-F7, you can define the server and the remote path used. The 
"   settings can be set for the buffer only or globally if you have many 
"   files to synchronize from/to the same directory. You can set both, and 
"   then buffer settings will preempt global settings. 
"   
"   Key mapping:
"
"     - <S-F5> and <S-F6>: allow you to update your file respectively from 
"       and to the server. Asks for the synchronization parameters if needed.
"     - <S-F7>: set the synchronization parameters.
"     - <S-F8>: display the synchronization parameters of the current buffer.
"   
"   Public functions:
"
"     - FtpGetParam(): display the synchronization parameters of the current 
"       buffer.
"     - FtpSetParam(...): set the synchronization parameters
"     - FtpSetServer(...): set the server parameter
"     - FtpSetPath(...): set the path parameter
"     - FtpUpdate(): put the buffer to the remote server
"     - FtpRefresh(): get the file from the remote server
"
"   Note that you can set both global parameters and buffer parameters
"   for a specific buffer which needs different parameters. The buffer 
"   settings are taken into account before the global ones.
"
"   Further more, through the FtpSetPath or FtpSetServer functions, you
"   can set only a specific path or server for a buffer, the second parameter
"   (path or server) being taken from the global variable.
"
"   Example:
"
"   With <S-F7> you set global parameters srv1 and path /home/user1.
"   Then for a specific buffer you type :call FtpSetPath() and set
"   /home/dummy2 as buffer path (or type :call FtpSetPath("/home/user2","b")).
"   Then using <S-F5> or <S-F6> on this buffer will use ftp://srv1/home/user2
"   and ftp://srv1/home/user1 for the other ones.
"
"   The protocol used is g:FtpSync_Proto (scp by default). Writing and reading 
"   are performed through netrw.vim plugin, i.e. as typing 
"   
"       :write <proto>://<server>/<path>/<file>; 
"       or 
"       :read <proto>://<server>/<path>/<file>; 
"
"	Installation:
"
"	Copy the script into your $VIM/vimfiles/plugin
"

if !exists("g:FtpSync_Proto") 
    let g:FtpSync_Proto="scp"
endif

map <silent> <S-F5> :call FtpRefresh()<CR>
map <silent> <S-F6> :call FtpUpdate()<CR>
map <silent> <S-F7> :call FtpSetParam()<CR>
map <silent> <S-F8> :call FtpGetParam()<CR>

function! FtpGetParam()

    let l:sep = ''
    
    if(!exists("b:FtpUpdateServer") && !exists("g:FtpUpdateServer"))
        echo "No synchronization parameters set"
        return
    endif

    if(exists("b:FtpUpdateServer"))
        let l:srv = b:FtpUpdateServer
    else
        let l:srv = g:FtpUpdateServer
    endif

    if(exists("b:FtpUpdatePath"))
        let l:path = b:FtpUpdatePath
    else
        let l:path = g:FtpUpdatePath
    endif

    if(l:path[0] != '/')
        let l:sep = '/'
    endif
    
    echo "Synchronize: " . g:FtpSync_Proto . "://" . l:srv . l:sep . l:path
    
endfunction

function! FtpSetParam(...)

    if a:0 != 3
        let l:server = input('Enter server name: ')
        let l:path = input('Enter path: ')
        let l:mode = input('Enter mode (g/b): ')
    else
        let l:server = a:1
        let l:path = a:2
        let l:mode = a:3
    endif

    if l:mode != ''
        if l:server != ''
            let {l:mode}:FtpUpdateServer = l:server
            let {l:mode}:FtpUpdatePath = l:path
        else
            unlet {l:mode}:FtpUpdateServer
            unlet {l:mode}:FtpUpdatePath
        endif
    endif

endfunction

function! FtpSetServer(...)

    if a:0 != 2
        let l:server = input('Enter server name: ')
        let l:mode = input('Enter mode (g/b): ')
    else
        let l:server = a:1
        let l:mode = a:2
    endif

    if l:mode != ''
        let {l:mode}:FtpUpdateServer = l:server
    endif

endfunction

function! FtpSetPath(...)

    if a:0 != 2
        let l:path = input('Enter path: ')
        let l:mode = input('Enter mode (g/b): ')
    else
        let l:path = a:1
        let l:mode = a:2
    endif

    if l:mode != ''
        let {l:mode}:FtpUpdatePath = l:path
    endif

endfunction

function! FtpUpdate()

    if(!exists("b:FtpUpdateServer") && !exists("g:FtpUpdateServer"))
        call FtpSetParam()
    endif

    if(exists("b:FtpUpdateServer"))
        let l:srv = b:FtpUpdateServer
    else
        let l:srv = g:FtpUpdateServer
    endif

    if(exists("b:FtpUpdatePath"))
        let l:path = b:FtpUpdatePath
    else
        let l:path = g:FtpUpdatePath
    endif

    execute "write " . g:FtpSync_Proto . "://" . l:srv . "/" . l:path . fnamemodify(bufname("%"), ":t")

endfunction

function! FtpRefresh()

    if(!exists("b:FtpUpdateServer") && !exists("g:FtpUpdateServer"))
        call FtpSetParam()
    endif

    if(exists("b:FtpUpdateServer"))
        let l:srv = b:FtpUpdateServer
    else
        let l:srv = g:FtpUpdateServer
    endif

    if(exists("b:FtpUpdatePath"))
        let l:path = b:FtpUpdatePath
    else
        let l:path = g:FtpUpdatePath
    endif

    norm 1GdG
    execute "Nread " . g:FtpSync_Proto . "://" . l:srv . "/" . l:path . fnamemodify(bufname("%"), ":t")
    norm 1Gdd

endfunction

" vim:ts=4:expandtab:sw=4
