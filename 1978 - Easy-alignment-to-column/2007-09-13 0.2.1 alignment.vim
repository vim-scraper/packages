" ---------------------------------------------------------------------
" -----  Key Mappings  ------------------------------------------------
" ---------------------------------------------------------------------

if !exists("g:mapleader")
    let g:mapleader=","
endif

exec "nmap ".g:mapleader."a :call AlignToMark( \"c\" )<CR>"
exec "nmap ".g:mapleader."A :call TryDetermineAlignColumn( \"c\" )<CR>"

" ---------------------------------------------------------------------
" -----  Implementation  ----------------------------------------------
" ---------------------------------------------------------------------

function! FancyEcho( text )
    let l:i = 0
    while l:i < len( a:text )
        if a:text[ l:i ] == "@"
            let l:i = l:i + 1
            if a:text[ l:i ] == "0"
                echohl None
            elseif a:text[ l:i ] == "1"
                echohl Directory
            elseif a:text[ l:i ] == "2"
                echohl WarningMsg
            elseif a:text[ l:i ] == "@"
                echon "@"
            endif
        else
            echon a:text[ l:i ]
        endif
        let l:i = l:i + 1
    endwhile
    echohl None
endfunction

function! BlinkColumn( times )
    let l:cc = &cursorcolumn
    for l:i in range( 1, a:times )
        set invcursorcolumn
        redraw
        sleep 100m
        set invcursorcolumn
        redraw
        sleep 100m
    endfor
    let &cursorcolumn = l:cc
endfunction

function! TryDetermineAlignColumn( mark )
    let l:pos = getpos( "." )
    normal k^
    let l:found = search( "\\s\\s", 'p', line(".") )
    if l:found == 0
        let l:found = search( "\\t", 'p', line(".") )
    endif
    if l:found == 0 && getline(".")[0] =~ "\\s"
        " sorry case: align at first non-white space.
        normal 0
        let l:found = 1
    endif
    if l:found > 0
        normal w
        exec "normal m" . a:mark
        call FancyEcho( "Set mark @1" . a:mark . "@0 at virtcol @1" . virtcol(".") . "@0." )
        call BlinkColumn( 2 )
    else
        call FancyEcho( "Unable to find pattern @1\\s\\s@0 or @1\\t@0 in previous line." )
    endif
    call setpos( ".", l:pos )
endfunction

let g:align_uses_tabs = 1
function! AlignToMark( markchar )
    let l:togoto = virtcol("'".a:markchar)
    if l:togoto == 0
        call FancyEcho( "Mark @1" . a:markchar . " @2not set@0."
        return
    endif
    let l:toinsert = l:togoto - virtcol(".")
    while l:toinsert < 0 && col(".") > 0
        let l:prev = getline(".")[ col(".")-2 ]
        if l:prev == '  ' || l:prev == ' '
            normal hx
            let l:toinsert = l:togoto - virtcol(".")
        else
            call FancyEcho( "Ran @2out of white space@0 to remove. You're on your own." )
            return
        endif
    endwhile
    " insert tabs until we jump past (or spot on) our goal
    if !&et && g:align_uses_tabs
        while l:toinsert > 0
            " that is i + a tab
            normal i    
            normal l
            let l:toinsert = l:togoto - virtcol(".")
        endwhile
        if l:toinsert < 0
            " undo last tab
            normal hx
            let l:toinsert = l:togoto - virtcol(".")
        endif
    endif
    " then fine-tune using spaces
    while l:toinsert > 0
        " that is i + a space
        normal i 
        normal l
        let l:toinsert -= 1
    endwhile
    call FancyEcho( "@1Aligned@0 to mark @1".a:markchar."@0 on virtual column @1".l:togoto."@0." )
    call BlinkColumn( 1 )
endfunction

