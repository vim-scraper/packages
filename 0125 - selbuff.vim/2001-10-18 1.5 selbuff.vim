"=============================================================================
" File : selbuff.vim
" Author : Todd Cosgrove (tcosgrov@twcny.rr.com)
" Last update : Fri Sep 14 2001
" Version : 1.5
"-----------------------------------------------------------------------------
" Selbuff.vim is an easy way to select a buffer.  It works by creating
" a window (n < (&lines/2) ? n : (&lines/2)) lines high listing all buffers 
" (except the selection temp buffer).  Moving the cursor to a line and pressing
" <CR> opens a window showing that buffer. Moving the cursor to a line and 
" pressing d delete's the buffer.  Moving the cursor to a line and pressing w 
" with save the file.  Pressing q within the select buffer window closes the
" window (the buffer is still loaded).
"
" Type <Leader>bl to use the buffer list.
"=============================================================================
" ToDo:
" -----
"  - 
"
" Modifications:
"---------------
" When        Who  What
" ----------  ---  -----------------------------------------------------------
" 03-24-2000  tjc  - Created Module.
" 10-18-2001  tjc  - Made some changes more in line with vim60.
"=============================================================================

" Has this already been loaded?
if exists( "loaded_selbuff" )
    finish
endif

let loaded_selbuff = 1

"---------------------------
" Map the function to a key
"---------------------------
if !hasmapto( '<Plug>StartSelBuff' )
    nmap <silent> <unique> <Leader>bl <Plug>SelBuff
endif
if !hasmapto( '<Plug>StartSelBuffAll' )
    nmap <silent> <unique> <Leader>bL <Plug>AllSelBuff
endif

nmap <silent> <unique> <script> <Plug>SelBuff :call <SID>Main( 0 )<C-M>
nmap <silent> <unique> <script> <Plug>AllSelBuff :call <SID>Main( 1 )<C-M>

" Var to save state
if !exists( "s:sblastbuffer" )
    let s:sblastbuffer = " "
endif
if !exists( "s:sbwinnr" )
    let s:sbwinnr = 1
endif
if !exists( "s:showall" )
    let s:showall = 0
endif    
if !exists( "s:sortbyname" )
    let s:sortbyname = 1
endif    

"-------------------------
" Main function
"-------------------------
function! <SID>Main( showall )

    " Is it the current buffer?
    if bufname( bufnr( "%" ) ) == "_selbuff.tmp"

        echo "Already in Select buffer."

        return

    else

        " Delete the buffer if necessary
        if bufexists( "_selbuff.tmp" ) != 0
            exe "bwipeout! _selbuff.tmp"
        endif

    endif

    let s:sblastbuffer = expand( "%" )
    let s:sbwinnr = winnr( )
    let s:showall = a:showall

    " Count buffers to figure out window height
    let sbbufCount = <SID>CountBuffers( )
    if sbbufCount == 1

        echo "Only buffer."
        return

    elseif sbbufCount > (&lines / 2)

        let sbbufCount = (&lines / 2)

    endif

    " If showall, add one for the buffer being created
    if s:showall
        let sbbufCount = sbbufCount + 1
    endif

    " Create the window for display/selection
    call <SID>CreateWindow( sbbufCount )

    setlocal nobuflisted
    setlocal nonumber
    setlocal buftype=nofile

    " Setup the key mappings
    call <SID>Initialize( )

    " Fill in the buffer list
    call <SID>ListBuffers( )

    return

endfunction

"----------------
" Initialization
"----------------
function! <SID>Initialize( )

    " These keys are used
    nmap <buffer> <silent> <CR>  :call <SID>KeySelect( )<CR>
    nmap <buffer> <silent> d     :call <SID>KeyDelete( )<CR>
    nmap <buffer> <silent> q     :call <SID>KeyQuit( )<CR>
    nmap <buffer> <silent> w     :call <SID>KeyWrite( )<CR>
    nmap <buffer> <silent> s     :call <SID>SortByName( 1 )<CR>

    " These need to be inactive
    nmap <buffer> <C-W> <Nop>
    nmap <buffer> <C-V> <Nop>
    nmap <buffer> <Del> <Nop> 
    nmap <buffer> <BS>  <Nop> 
    nmap <buffer>     <Nop>
    nmap <buffer> a     <Nop>
    nmap <buffer> A     <Nop>
    nmap <buffer> c     <Nop>
    nmap <buffer> C     <Nop>
    nmap <buffer> D     <Nop>
    nmap <buffer> i     <Nop>
    nmap <buffer> I     <Nop>
    nmap <buffer> J     <Nop>
    nmap <buffer> o     <Nop>
    nmap <buffer> O     <Nop>
    nmap <buffer> p     <Nop>
    nmap <buffer> P     <Nop>
    nmap <buffer> r     <Nop>
    nmap <buffer> R     <Nop>
    nmap <buffer> S     <Nop>
    nmap <buffer> u     <Nop>
    nmap <buffer> U     <Nop>
    nmap <buffer> v     <Nop>
    nmap <buffer> V     <Nop>
    nmap <buffer> x     <Nop>
    nmap <buffer> X     <Nop>
    nmap <buffer> ~     <Nop>
    nmap <buffer> .     <Nop>
    nmap <buffer> ,     <Nop>
    nmap <buffer> =     <Nop>

    return

endfunction

"-------------------------
" Count the # of buffers
"-------------------------
function! <SID>CountBuffers( )

    let bufcount = 0

    " Count the number of buffers
    let i = 1
    while i <= bufnr( "$" )

        " Don't count the selection buffer
        if (s:showall && bufexists( i )) || buflisted( i )
            
            let bufcount = bufcount + 1

        endif
        
        let i = i + 1

    endwhile

    return bufcount

endfunction

"-------------------------
" Create/Open the window
"-------------------------
function! <SID>CreateWindow( height )

    " move to the bottom window
    exe "wincmd b"

    " create the window
    exe "botright " . a:height . "sp! " . "_selbuff.tmp"

    return

endfunction

"---------------------------
" Clear buffer
"---------------------------
function! <SID>ClearBuffer( )
    
    " Delete everything
    exe "norm! 1G\"_dG"

    return

endfunction

"---------------------------
" Add buffer list to window
"---------------------------
function! <SID>ListBuffers( )

    let i = 1
    let windowline = 1
    let cursorline = 1

    " Loop through all of the buffers
    while i <= bufnr( "$" )

        if (s:showall && bufexists( i )) || buflisted( i )

            " Format the first column
            let bnstr = i
            while strlen( bnstr ) < 5

                let bnstr = " " . bnstr

            endwhile

            " Format the second column (30 chars)
            " Display [No File] if empty buffer
            let fnstr = fnamemodify( bufname( i ), ":t" )
            if strlen( fnstr ) == 0

                let fnstr = "[No File]"

            endif

            " Pad to 35 columns with spaces
            while strlen( fnstr ) < 35

                let fnstr = fnstr . " "

            endwhile

            " Set the current buffer flag
            let bmstr = " "
            if bufname( i ) == s:sblastbuffer

                let bmstr = "%"
                let cursorline = windowline

            endif

            " Set the unlisted/hidden/active
            if !buflisted( i )
                let bmstr = bmstr . "u"
            elseif buflisted( i ) && bufwinnr( i ) == -1
                let bmstr = bmstr . "h"
            else
                let bmstr = bmstr . "a"
            endif

            " Set the modified flag
            let bmstr = bmstr . <SID>IsModified( bufnr( i ) )

            " Write the line to the buffer
            call append( line( "$" ), bnstr . " " . bmstr
                       \ . " "  . fnstr . "  " .
                       \ fnamemodify( bufname( i ), ":p:h" ) . "/" )

            let windowline = windowline + 1

        endif

        let i = i + 1

    endwhile

    " Delete the first line
    exe "norm! 1G\"_dd"

    " Sort the buffer based on s:sortbyname
    call <SID>SortByName( 0 )

    " Put the cursor on the line
    exe "norm! 1G"
    
    return

endfunction

"---------------------------------
" Determine if buffer is modified
" (for flags field)
"---------------------------------
function! <SID>IsModified( bufnum )

    if getbufvar( a:bufnum, "&mod" )
        
        return "+"

    else

        return " "

    endif

endfunction

"-------------------
" Select entry <CR>
"-------------------
function! <SID>KeySelect( )

    let winswitch = winnr( ) - s:sbwinnr

    " Get the buffer line
    let bufline = getline( "." )

    " Get the buffer number from the current line
    let bufnum = substitute( strpart( bufline, 0, 5 ), " *", "", "" ) + 0

    "switch to the previous window
    let i = 1
    while i <= winswitch
        exe "wincmd W"
        let i = i + 1
    endwhile

    " Switch to the buffer
    exe "buffer " . bufnum

    " Delete the selection buffer
    exe "bwipeout! _selbuff.tmp"

    " Was it the [No File] buffer
    let fnstr = fnamemodify( bufname( "%" ), ":t" )
    if strlen( fnstr ) == 0

        let fnstr = "[No File]"

    endif

    echo "Selected buffer: " . fnstr

    return

endfunction

"--------------------
" Quit selecting <q>
"--------------------
function! <SID>KeyQuit( )

    let winswitch = winnr( ) - s:sbwinnr

    "switch to the previous window
    let i = 1
    while i <= winswitch
        exe "wincmd W"
        let i = i + 1
    endwhile

    " Delete the selection buffer
    exe "bwipeout! _selbuff.tmp"

    echo "Exited selection buffer."

    return

endfunction

"----------------------
" Delete selection <D>
"----------------------
function! <SID>KeyDelete( )

    " Get the buffer line
    let bufline = getline( "." )

    " Get the buffer number from the current line
    let bufnum = substitute( strpart( bufline, 0, 5 ), " *", "", "" ) + 0

    " Has buffer changed ("+" char in column 8)?
    let bufchanged = strpart( bufline, 7, 1 ) 

    if bufchanged == "+"
        
        let choice = confirm( "Buffer changed, delete?", "&Yes\n&No\n&Cancel" )
        if choice == "1"

            exe "bwipeout! " . bufnum

        endif

    else

        exe "bwipeout " . bufnum

    endif

    " Recreate the selection buffer
    exe "bwipeout _selbuff.tmp"

    " Start all over again
    call <SID>Main( s:showall )

    return

endfunction

"----------------------
" Write selection <w>
"----------------------
function! <SID>KeyWrite( )

    " Get the buffer line
    let bufline = getline( "." )

    " Get the buffer number
    let bufnum = substitute( strpart( bufline, 0, 5 ), " *", "", "" ) + 0

    " Has buffer changed ("+" char in column 8)?
    let bufchanged = strpart( bufline, 7, 1 ) 

    if bufchanged == "+"

        let sbfilename = bufname( bufnum )

        " If no file, prompt for file name
        if strlen( sbfilename ) == 0

            let sbfilename = input( "Enter file name: " )

        endif

        " Switch to the selected buffer
        exe "buffer " . bufnum
        
        " Write the buffer
        exe "write! " . sbfilename
        
        " Switch back to the selection buffer
        exe "buffer " . bufnr( "_selbuff.tmp" )

        " Erase selection buffer contents
        call <SID>ClearBuffer( )

        " Recreate selection buffer list
        call <SID>ListBuffers( )

     else

        if strlen( bufname( bufnum ) ) == 0

            echo "Buffer not changed: [No File]"

        else

            echo "Buffer not changed: " . bufname( bufnum )

        endif

    endif

    return

endfunction

"-----------
" Sort list
"-----------
function! <SID>SortByName( toggle )

    if a:toggle == 1
        if s:sortbyname == 1
            let s:sortbyname = 0
        else
            let s:sortbyname = 1
        endif
    endif

    if s:sortbyname == 1
       if has( "unix" )
           exe "1,$!sort -k 3 -b -f"
       elseif has("win32")
           exe "1,$!sort /+11"
       endif
    else
        exe "1,$!sort"
    endif

endfunction
