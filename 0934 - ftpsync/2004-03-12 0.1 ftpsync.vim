map <silent> <S-F5> :call FtpRemoteSync()<CR>
map <silent> <S-F6> :call FtpSync()<CR>
map <silent> <S-F7> :call FtpSetParam()<CR>

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
            let {l:mode}:FtpSyncServer = l:server
            let {l:mode}:FtpSyncPath = l:path
        else
            unlet {l:mode}:FtpSyncServer
            unlet {l:mode}:FtpSyncPath
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
        let {l:mode}:FtpSyncServer = l:server
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
        let {l:mode}:FtpSyncPath = l:path
    endif

endfunction

function! FtpSync()

    if(!exists("b:FtpSyncServer") && !exists("g:FtpSyncServer"))
        call FtpSetParam()
    endif

    if(exists("b:FtpSyncServer"))
        let l:srv = b:FtpSyncServer
    else
        let l:srv = g:FtpSyncServer
    endif

    if(exists("b:FtpSyncPath"))
        let l:path = b:FtpSyncPath
    else
        let l:path = g:FtpSyncPath
    endif

    execute "write ftp://" . l:srv . "/" . l:path . fnamemodify(bufname("%"), ":t")

endfunction

function! FtpRemoteSync()

    if(!exists("b:FtpSyncServer") && !exists("g:FtpSyncServer"))
        call FtpSetParam()
    endif

    if(exists("b:FtpSyncServer"))
        let l:srv = b:FtpSyncServer
    else
        let l:srv = g:FtpSyncServer
    endif

    if(exists("b:FtpSyncPath"))
        let l:path = b:FtpSyncPath
    else
        let l:path = g:FtpSyncPath
    endif

    norm 1GdG
    execute "Nread ftp://" . l:srv . "/" . l:path . fnamemodify(bufname("%"), ":t")
    norm 1Gdd

endfunction

" vim:ts=4:expandtab:sw=4
