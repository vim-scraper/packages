" Vim script -- allows for typing symbols composed of several characters
" General: {{{1
" File:         larlet.vim
"               (large letters . vim)
" Vimscript:    #1357
" Author:       Andy Wokula, anwoku@yahoo.de
" Last Change:  2007 Aug 21
" Version:      0.04
" Vim Version:  Vim 6.1

" Installation:
"       :source larlet.vim
"   for current buffer when needed.  Mappings (including F8) and options
"   (except 'lcs') are buffer-local.
"
" Releases:
"   v0.10: actually v0.1 (numbering error ;)
"   v0.04: made script less intrusive, minor new features, dropped menu

" Usage:
"   Press F8 (or i_F8) to toggle mapping of all the Insertmode keys
"   ... start typing in Insertmode ...
"
"   For available keys see s:keymap variable.
"
"   Keys for editing:
"   i_<BS>      (= Backspace) delete symbol from the end of the dline
"   i_Enter     append new dline below (+ extra line)

" Customization:
"   :SpaceSet [{letterspace} [{extra-wordspace}]]
"       {letterspace} - set number of spaces between symbols (0 or higher)
"       wordspace     - number of spaces for a symbol-space
"                       = 2 * {letterspace} + {extra-wordspace}
"       Default for {extra-wordspace}:
"           2, if {letterspace}==0
"           1, else
"       Without arguments, print current settings.
"
" Terms:
"   dline - double line - two lines consumed by symbols
"   dspace - two spaces, one on line0:   X   X   X           (3 possible
"                    another on line1:  X    X    X           arrangements)
"           Backspace looks for separating dspaces.
"   symbol-space - a symbol-sized space (two lines, varying nr of columns)
"
" Note: lookout for non-breaking spaces (160) in symbolmap
" }}}

" Script Encoding: {{{1
" recommended encoding:
scriptenc latin1
" also this should convert the Symbolmap (some chars above 127) to your
" encoding if different and make the multibyte check work

" Some Checks: {{{1
" detect multibyte 'encoding'
if strlen("¯´ó") != 3
    echomsg "Larlet: cannot handle multibyte encodings"
    finish
endif

" Symbolmap: {{{1

" the following keys are mapped to symbols:
let s:keymap =    "abcdefghijklmnopqrstuvwxyz.,;!?=+-*:'`#()[]{}^~%$&0123456789/\\\"_@<>|EL"
"                    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .
let s:alpha0 =    '  /|   /)   /¯   /\   /¯¯  /¯¯ /¯_   / /   /    /   //   /   /|/) /|/  /¯)  /¯)  /¯)   /¯)  (¯  -/-   / / | /  /  /  |/ (_/  _7               o    !!   ¯)  --    |   __   .".   o   /    \   _||_   /¯    )   /¯    7  _/¯   ¯|  ´`   /\/  o/   (|`   \)  |¯|  /|   ´¯)   ¯/   /,   |¯¯  /   ´¯/   (¯) (_¯)   /  \     //        _    /    \    |    _   ¯\  '
let s:alpha1 =    ' /¯|  /_)  (_   /_/  (_¯  / ¯ (__/  /¯7   /   (/   /¯\  /_  /  / / /  (_/  / ¯  (_X   /¯\  __)  /    (_/  |/  (/|/  /|   /   /_     o    /   /     o    ó   --   ¯|¯       ´"`   o            -||-  (    _/   /_   _/   |_   _/¯            /o   _|)  (_X  |_|  _|_   /_  ,_)   ¯|¯  _¯) (_¯)  /   (_¯)   /  /´    \        ___  (d_   \    /    |   (=_  /¯\ '
"                    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '
" Characters above 127:
" "¯"   macron (opposed to "_")
" "´"   acute accent (opposed to grave accent "`")
" "ó"   letter "o" with acute
" " "   non-breakable space
"
"   .    .    .    .    .    .    .    .  
" |    /¯¯¯\|¯¯¯)/_¯¯ ¯¯|¯¯|   | ¯|¯ |\ /|
" |___ |¯¯¯||¯¯\ |____  |   \_/  _|_ | ^ |
"   '    '    '    '    '    '    '    '  
" LARLET.VIM - just for testing

" Map F8: {{{1
nno <plug>Larlet_ToggleKeymap :call <SID>ToggleKeyBinding()<cr>

if !hasmapto("Larlet_ToggleKeymap", "ni")
    " ? hasmapto("\<plug>...") always returns 0, if key is buffer local
    try
        nmap <buffer><unique> <F8> <plug>Larlet_ToggleKeymap
        imap <buffer><unique> <F8> <c-o><plug>Larlet_ToggleKeymap
    catch /:E227:/
        " F8 is already mapped otherwise
        echomsg "Larlet: You should map a key to <plug>Larlet_ToggleKeymap"
    endtry
endif

" Handle Spaces: {{{1
function! s:spacerepeat(len)
    if a:len<=0
        return ""
    endif
    let char = "       "
    let s = char
    let l = strlen(char)
    while l<a:len
        let s = s.s
        let l = l+l
    endw
    return strpart(s, 0,a:len)
endfunction

let s:spaceline = s:spacerepeat(80)
function! s:spaces(n)
    if a:n <= 80
        return strpart(s:spaceline, 0, a:n)
    else
        return s:spacerepeat(a:n)
    endif
endfunction

function! s:LTrimWhite(s)
    return substitute(a:s, '^ *', '', '')
endfunction

function! s:RTrimWhite(s)
    return substitute(a:s, ' *$', '', '')
endfunction

" Append Symbol: {{{1
" Append Symbol at the end of the dline:
" (insert minimal whitespace, "kerning")
function! <SID>ASymbol(bidx)
    " bidx  symbol index
    let lnr = line(".")
    if lnr == line("$")
        call append("$", "")
    endif
    if lnr == line("w$")    " ignored in Vim6
        exe "norm! \<c-e>"
    endif

    let line0 = s:RTrimWhite(getline(lnr))
    let line1 = s:RTrimWhite(getline(lnr+1))
    let len_dif = strlen(line1) - strlen(line0)

    " get the definition lines of the symbol:
    let alidx = a:bidx*5
    let al0 = s:RTrimWhite(strpart(s:alpha0, alidx,5))
    let al1 = s:RTrimWhite(strpart(s:alpha1, alidx,5))

    if s:prespace>0
        let symspace = s:prespace
    else
        let symspace = b:Larlet_SpaceLen
    endif

    if al0==""
        let al1 = s:spaces(symspace) . s:LTrimWhite(al1)
        if len_dif<-1
            let al1 = s:spaces(-len_dif-1) . al1
        endif
    elseif al1==""
        let al0 = s:spaces(symspace) . s:LTrimWhite(al0)
        if len_dif>0
            let al0 = s:spaces( len_dif) . al0
        endif
    else
        let asi0 = match(al0, '\S')
        let asi1 = match(al1, '\S')
        let symlen_dif = asi1 - asi0
        let exsp = len_dif - symlen_dif

        let al0 = s:spaces(symspace) . s:LTrimWhite(al0)
        let al1 = s:spaces(symspace) . s:LTrimWhite(al1)

        if exsp > 0
            let al0 = s:spaces(exsp) . al0
        endif
        if exsp < 0
            let al1 = s:spaces(-exsp) . al1
        endif
    endif

    call setline(lnr  , line0 . al0)
    call setline(lnr+1, line1 . al1)
    let s:prespace = 0

    call cursor(".", col("$")-1)
endfunction

" Spacekey: {{{1
" Add one symbol-space as visual hint.  spaces are trimmed when inserting the
" next symbol, therefore remember the amount of spaces to be inserted then.
function! <SID>SpaceKey()
    " only a hint for the user:
    let hint = s:spaces(b:Larlet_Wordspace)
    call setline(".", getline(".") . hint)
    normal! $
    let s:prespace = s:prespace + b:Larlet_Wordspace
endfunction

" Enterkey: {{{1
" Enter: append a new dline below
function! <SID>EnterKey()
    " dont preserve space:
    let s:prespace = 0

    let lnr = line(".")
    if lnr==line("$")
        call append("$", "")
    else
        +1
    endif
    put _
    put _
    put _
    -1
endfunction

" Backspacekey: {{{1
" Backspace:
" Delete symbols or space chars from the end of the current dline
"
" dline: one of line0 and line1  or  line0 together with line1
" dspace: (double space) is a SpaceChar together with a SpaceChar <sc> on the
" next line.  the vertical position of <sc> is either the same or one character
" to the left (due to the fact that most of the symbols look italic)

function! <SID>BackspaceKey()
    let lnr = line(".")

    " catch some errors:
    if lnr == line("$")
        return
    endif

    let line0 = getline(lnr)
    let line1 = getline(lnr+1)
    let len0 = strlen(line0)
    let len1 = strlen(line1)

    " if whole dline is empty: exit
    if !len0 && !len1
        return
    endif

    let ws = b:Larlet_Wordspace

    " if there is a space character at the end of the dline OR prespace>0
    if line0[len0-1] == " " || line1[len1-1] == " " || s:prespace>0
        " only delete SpaceChars
        " delete at most wordspace SpaceChars at a time
        if len0>ws
            let line0 = strpart(line0, 0,len0-ws) . s:RTrimWhite(strpart(line0, len0-ws,ws))
        else
            let line0 = s:RTrimWhite(strpart(line0, 0,len0))
        endif
        if len1>ws
            let line1 = strpart(line1, 0,len1-ws) . s:RTrimWhite(strpart(line1, len1-ws,ws))
        else
            let line1 = s:RTrimWhite(strpart(line1, 0,len1))
        endif

        " if prespace>0: decrement prespace
        if s:prespace>0
            let s:prespace = s:prespace-b:Larlet_Wordspace
        endif

    else
        " delete symbol characters

        " make line0 and line1 of equal length (append SpaceChars):
        if len1>len0
            let line0 = line0 . s:spaces(len1-len0)
            let len0 = len1
        endif
        if len0>len1
            let line1 = line1 . s:spaces(len0-len1)
            let len1 = len0
        endif

        " search left for the first dspace within 5 characters:
        let dsp0 = -1   " not found
        let dsp1 = -1
        let i = len0-1
        while i>=len0-1-5
            if i<0
                let dsp0 = 0
                let dsp1 = 0
                break
            endif
            if line0[i] == " "
                if line1[i] == " "
                    let dsp0 = i
                    let dsp1 = i
                elseif line1[i-1] == " "
                    \ && (line0[i-1]!="\\" && line1[i]!="\\")
                    " for: \VM
                    let dsp0 = i
                    let dsp1 = i-1
                elseif line1[i+1] == " "
                    \ && (line0[i+1]!="/" && line1[i]!="/")
                    " rule of thumb for: fhijmnpuvw;()]/
                    let dsp0 = i
                    let dsp1 = i+1
                endif
            endif
            if dsp0>=0
                break
            endif
            let i = i-1
        endwhile

        if dsp0>=0
            " dspace found
            let line0 = s:RTrimWhite(strpart(line0, 0,dsp0))
            let line1 = s:RTrimWhite(strpart(line1, 0,dsp1))

        else
            " seems, that SpaceLen is zero -> delete one character from line0
            " and line1 (dspace-like) + Righttrim
            let zes = line0[len0-1] == " "
            let oes = line1[len0-1] == " "

            let len1 = len1-1
            if oes && !zes
                let len1 = len1-1
            endif
            if oes
                let len0 = len0-1
            endif
            let line0 = s:RTrimWhite(strpart(line0, 0,len0))
            let line1 = s:RTrimWhite(strpart(line1, 0,len1))
        endif
    endif

    call setline(lnr  , line0)
    call setline(lnr+1, line1)

    call cursor(".", col("$"))
endfunction


" KeyMapping For Symbols: {{{1

" map/unmap all the s:keymap keys:
function! <SID>ToggleKeyBinding()
    if ! exists("b:Larlet_KeyBinding")
        let b:Larlet_KeyBinding = 0
    endif

    if b:Larlet_KeyBinding==0
        " 0 - no letter mapped
        let i = 0
        while i<strlen(s:keymap)
            let k = escape(s:keymap[i], '|')
            execute "ino <buffer><silent> ".k." <esc>:call <SID>ASymbol(".i.")<cr>a"
            let i = i+1
        endw
        " map special keys:
        ino <buffer><silent> <cr>    <c-o>:call <SID>EnterKey()<cr>
        ino <buffer><silent> <space> <c-o>:call <SID>SpaceKey()<cr>
        ino <buffer><silent> <c-h>   <c-o>:call <SID>BackspaceKey()<cr>
        imap <buffer>        <bs>    <c-h>
        let b:Larlet_KeyBinding = 1
        echo "Large symbol keys (for insert mode) mapped!"

    elseif b:Larlet_KeyBinding==1
        " 1 - mapped
        let i = 0
        while i<strlen(s:keymap)
            call s:Biunmap(s:keymap[i])
            let i = i+1
        endw
        call s:Biunmap("<cr>")
        call s:Biunmap("<space>")
        call s:Biunmap("<c-h>")
        call s:Biunmap("<bs>")
        let b:Larlet_KeyBinding = 0
        echo "All symbol keys (for insert mode) unmapped."

    endif
endfunction

function! s:Biunmap(key)
    execute "silent! iunmap <buffer> " . escape(a:key, '|')
endfunction

" Options (SpaceSet): {{{1

command! -buffer -nargs=* SpaceSet call s:SpaceSet(<f-args>)

function! s:SpaceSet(...)
    if !a:0
        let b:Larlet_SpaceLen b:Larlet_ExtraSpaces
        return
    endif
    let b:Larlet_SpaceLen = a:1+0
    if a:0>=2
        let b:Larlet_ExtraSpaces = a:2+0
    else
        " some default values
        let b:Larlet_ExtraSpaces = b:Larlet_SpaceLen==0 ? 2 : 1
    endif
    let b:Larlet_Wordspace = 2*b:Larlet_SpaceLen + b:Larlet_ExtraSpaces
endfunction

" Vim Options:

" Bufferlocal Options:
setlocal expandtab
" don't use hardtabs -- "No" tabs:
setlocal list
setlocal nowrap

" Global Options:
let save_lcs = &listchars
set listchars=tab:No,trail:_

" Restore user's global options:
"   :let &lcs=save_lcs|unlet save_lcs

" Initial Values: {{{1

" SpaceSet b:SpaceLen, b:ExtraSpaces
SpaceSet 1 1

let s:prespace = 0
" more or less temporary - not per buffer

" Modeline: {{{1
" vim: set tw=0 ts=8 sts=4 et nowrap fdm=marker:
