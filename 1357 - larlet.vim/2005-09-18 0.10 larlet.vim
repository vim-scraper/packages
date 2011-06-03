" File:         larlet.vim
"               (large letters . vim)
" Author:       Andy Wokula, anwoku@yahoo.de
" Last Change:  2005 Sep 18
" Version:      0.10

" Description:  Enable Insert mode to generate larger symbols (à la unix banner
"               utility) - while typing.  It's rather fun than a utility.
"
"  /¯¯ /|/  / /¯/ |/ !!        ... and drop me a line on
" /_¯ / / (/ /_/  /  o         improvements and ideas!
"
" Symbol size is fixed: 2 lines x 5 columns.  Symbols are arranged tightly,
" "type-setting" is not monospace.
"
" Key Mapping:
" - Insert mode keys will be remapped, affected keys are defined in s:keymap
"   (plus some special keys: Enter, Space, Backspace).
" - F8 toggles remapping on and off (default: off)
"
" Editing Possibilities: (Insert mode)
" - symbols will always be appended at the end of the dline (double line)
" - Space preserves a number of space characters to put before the next symbol
" - Enter appends a new dline and jumps there
" - Backspace deletes symbols from the end of the dline (by looking for a
"   dspace)
"
" Options:
"  - a menu "Larlet" will be added for setting options.
"  - user can toggle between his and Larlet's Vim options
"
" TODO: too much


nn  <buffer> <F8> :call <SID>ToggleKeyBinding()<cr>
ino <buffer> <F8> <c-o>:call <SID>ToggleKeyBinding()<cr>

let s:keymap =    "Mabcdefghijklmnopqrstuvwxyz.,;!?=+-*:'`#()[]{}^~%$&0123456789äöüß/\\\""
"                    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .    .
let s:alpha0 =    '#####  /|   /)   /"   /\   /¯¯  /¯¯ /¯_   / /   /    /   //   /   /|/) /|/  /¯/  /¯)  /¯/   /¯)  (¯  -/-   / / | /  /  / |/  |/   _7               o    !!   ¯)  --   _|_  __   \/    o   /    \   _||_   /´    |   /¯    7  _/¯   ¯|  ´`  /¯\_/ o/   (|`   \)  |¯|  /|   ´¯)   ¯/   /,   |¯   /   ´¯/   ()  (¯)   "/  _"   /"/  |)    ,´  \     //   '
let s:alpha1 =    '##### /¯|  /_)  /_   /_/  /_¯  / ¯ (__/  /¯7   /   (/   /¯\  /_  /  / / /  /_/  / ¯  /_X   /¯\  __)  /    (_/  |/  (/|/ /|   /    /_     o    /   /     o    ó   --    |        /\    o            -||-  |    ,/   /_   _/   |_   _/¯            /o   _|)  (_X  |_|  _|_   /_  ,_)   ¯|¯  _)  (_)   /   (_)   /  /¯/  /_/  (_/   |´) .´     \         '
"                    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '    '


" return a string of <char>s with length <len>:
" <char> can be a string to allow for patterns
fun! SpaceStr(len, char)
    if a:len==0
        return ""
    endif
    if a:char==""
        let a:char = " "
    endif
    let s = a:char
    let l = strlen(a:char)
    while l<a:len
        let s = s.s
        let l = l+l
    endw
    return strpart(s, 0,a:len)
endf

fun! LTrimWhite(s)
    return substitute(a:s, '^'.s:whitepattern.'*', '', '')
endf

fun! RTrimWhite(s)
    return substitute(a:s, s:whitepattern.'*$', '', '')
endf


" Append Symbol at the end of the line:
" (insert minimal whitespace, "kerning")
fun! <SID>ASymbol(bidx)
    " bidx  symbol index
    let lnr = line(".")
    if lnr == line("$")
        call append("$", "")
    endif

    let line0 = RTrimWhite(getline(lnr))
    let line1 = RTrimWhite(getline(lnr+1))
    let lendif = strlen(line1) - strlen(line0)

    " get the definition lines of the symbol:
    let alidx = a:bidx*5
    let al0 = RTrimWhite(strpart(s:alpha0, alidx,5))
    let al1 = RTrimWhite(strpart(s:alpha1, alidx,5))

    if s:prespace>0
        let symspace = s:prespace
    else
        let symspace = b:SpaceLen
    endif

    if al0==""
        let al1 = strpart(s:spaceline, 0,symspace) . LTrimWhite(al1)
        if lendif<-1
            let al1 = strpart(s:spaceline, 0,-lendif-1) . al1
        endif
    elseif al1==""
        let al0 = strpart(s:spaceline, 0,symspace) . LTrimWhite(al0)
        if lendif>0
            let al0 = strpart(s:spaceline, 0, lendif) . al0
        endif
    else
        let asi0 = 0 | while al0[asi0] == " " | let asi0 = asi0+1 | endw
        let asi1 = 0 | while al1[asi1] == " " | let asi1 = asi1+1 | endw

        let al0 = strpart(s:spaceline, 0,symspace) . LTrimWhite(al0)
        let al1 = strpart(s:spaceline, 0,symspace) . LTrimWhite(al1)

        let exsp = lendif - (asi1 - asi0)

        if exsp > 0
            let al0 = strpart(s:spaceline, 0,exsp) . al0
        endif
        if exsp < 0
            let al1 = strpart(s:spaceline, 0,-exsp) . al1
        endif
    endif

    call setline(lnr  , line0 . al0)
    call setline(lnr+1, line1 . al1)
    let s:prespace = 0

    call cursor(lnr, 99999)
endf


" special insertmode keys:

" Space: trailing space in the text doesnt matter.  what matters is how often
" space-key is pressed - counted in prespace. this makes spacing exact, but may
" lead to unexpected behaviour, e.g. press space several times, then change the
" line and go on with some other characters - the space will be inserted before
" them
fun! <SID>SpaceKey()
    " this gives only a hint to the user:
    exec "normal $\"=strpart(s:spaceline, s:prespace,b:wordspace)\rp"

    let s:prespace = s:prespace + b:wordspace
endf


" Enter: append a new dline below
fun! <SID>EnterKey()
    " dont preserve space:
    let s:prespace = 0

    let lnr = line(".")
    let cnr = col(".")
    if lnr==line("$")
        call append("$", "")
    else
        +1
    endif
    " do not remove the following empty lines and the dot:
    append



.
    -1
endf


" Backspace:
" Delete symbols or space chars from the end of the current dline
"
" dline: one of line0 and line1  or  line0 together with line1
" dspace: (double space) is a SpaceChar together with a SpaceChar <sc> on the
" next line.  the vertical position of <sc> is either the same or one character
" to the left (due to the fact that most of the symbols look italic)

fun! <SID>BackspaceKey()
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
    if !char2nr(line0[0]) && !char2nr(line1[0])
        return
    endif

    let ws = b:wordspace

    " if there is a space character at the end of the dline OR prespace>0
    if line0[len0-1] =~ s:whitepattern || line1[len1-1] =~ s:whitepattern || s:prespace>0
        " only delete SpaceChars
        " delete at most wordspace SpaceChars at a time
        if len0>ws
            let line0 = strpart(line0, 0,len0-ws) . RTrimWhite(strpart(line0, len0-ws,ws))
        else
            let line0 = RTrimWhite(strpart(line0, 0,len0))
        endif
        if len1>ws
            let line1 = strpart(line1, 0,len1-ws) . RTrimWhite(strpart(line1, len1-ws,ws))
        else
            let line1 = RTrimWhite(strpart(line1, 0,len1))
        endif

        " if prespace>0: decrement prespace
        if s:prespace>0
            let s:prespace = s:prespace-b:wordspace
        endif

    else
        " delete symbol characters

        " make line0 and line1 of equal length (append SpaceChars):
        if len1>len0
            let line0 = line0 . strpart(s:spaceline, 0,len1-len0)
            let len0 = len1
        endif
        if len0>len1
            let line1 = line1 . strpart(s:spaceline, 0,len0-len1)
            let len1 = len0
        endif

        " search left for the first dspace within 5 characters:
        let dsp0 = -1   " not found
        let dsp1 = -1
        let i = len0-1
        while i>=len0-1-5
            if line0[i] =~ s:whitepattern
                if line1[i-1] =~ s:whitepattern
                    let dsp0 = i
                    let dsp1 = i-1
                endif
                if line1[i] =~ s:whitepattern
                    let dsp0 = i
                    let dsp1 = i
                endif
            endif
            if dsp0>=0
                break
            endif
            let i = i-1
        endw

        if dsp0>=0
            " dspace found
            let line0 = RTrimWhite(strpart(line0, 0,dsp0))
            let line1 = RTrimWhite(strpart(line1, 0,dsp1))

        else
            " seems, that SpaceLen is zero -> delete one character from line0
            " and line1 (dspace-like) + Righttrim
            let zes = line0[len0-1] =~ s:whitepattern
            let oes = line1[len0-1] =~ s:whitepattern

            let len1 = len1-1
            if oes && !zes
                let len1 = len1-1
            endif
            if oes
                let len0 = len0-1
            endif
            let line0 = RTrimWhite(strpart(line0, 0,len0))
            let line1 = RTrimWhite(strpart(line1, 0,len1))
        endif
    endif

    call setline(lnr  , line0)
    call setline(lnr+1, line1)

    call cursor(lnr, 99999)
endf



fun! Biunmap(key)
    execute "silent! iunmap <buffer> " . a:key
endf

" map/unmap all the s:keymap keys:
fun! <SID>ToggleKeyBinding()
    if ! exists("b:KeyBinding")
        let b:KeyBinding = 0
    endif

    if b:KeyBinding==0
        " 0 - no letter mapped
        let i = 0
        while i<strlen(s:keymap)
            let k = s:keymap[i]
            execute "ino <buffer> <silent> ".k." <esc>:call <SID>ASymbol(".i.")<cr>a"
            let i = i+1
        endw
        " map special keys:
        ino <buffer> <silent> <cr>    <c-o>:call <SID>EnterKey()<cr>
        ino <buffer> <silent> <space> <c-o>:call <SID>SpaceKey()<cr>
        ino <buffer> <silent> <bs>    <c-o>:call <SID>BackspaceKey()<cr>
        let b:KeyBinding = 1
        echo "Large symbol keys (for insert mode) mapped!"

    elseif b:KeyBinding==1
        " 1 - mapped
        let i = 0
        while i<strlen(s:keymap)
            call Biunmap(s:keymap[i])
            let i = i+1
        endw
        call Biunmap("<cr>")
        call Biunmap("<space>")
        call Biunmap("<bs>")
        let b:KeyBinding = 0
        echo "All symbol keys (for insert mode) unmapped."

    else
    endif
endf

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Change Options:

" set b:SpaceLen consistently
fun! SetSpaceLen(nsl)
    let b:SpaceLen = a:nsl
    " hmm, needs SetExtraSpaces():
    let b:wordspace = 2*b:SpaceLen + b:ExtraSpaces
endf

" set b:SpaceChar consistently
fun! SetSpaceChar(nsc)
    if !IsValidSpaceChar(a:nsc)
        echohl ErrorMsg
        echo "\rSpaceChar must not contain characters used for symbols"
        echohl None
        if !exists("b:SpaceChar")
            let b:SpaceChar = " "
        endif
    else
        let b:SpaceChar = a:nsc
    endif

    " actual string space characters are taken from:
    let s:spaceline = SpaceStr(500, b:SpaceChar)

    " atomic pattern that matches any of the character(s) in SpaceChar.  it
    " always is case sensitive and contains " ":
    let s:whitepattern = '\C[ '. escape(b:SpaceChar, '^]-\') .']'
endf

" set b:ExtraSpaces consistently
fun! SetExtraSpaces(nes)
    let b:ExtraSpaces = a:nes
    " hmm, this needs SetSpaceLen():
    let b:wordspace = 2*b:SpaceLen + b:ExtraSpaces
endf

" check if SpaceChar does NOT contain characters used for symbols:
fun! IsValidSpaceChar(sc)
    let sp = "[" . escape(substitute(a:sc, '\s', '', 'gI'), '^]-\') . "]"
    " )
    if substitute(s:alpha0, '\C.\{-}'.sp.'.*', '', '') == ""
        return 0
    endif
    if substitute(s:alpha1, '\C.\{-}'.sp.'.*', '', '') == ""
        return 0
    endif
    return 1
endf


" Vim Options:

" Backup Vim options:
fun! BackupOptions()
    return 
    \" so=".&so." ".(&list?"":"no")."list lcs=".escape(&lcs,'\ |')
    \." ".(&wrap?"":"no")."wrap ch=".&ch
endf

" do this only once:
if !exists("b:larlet_options_set")
    " backup current user's Vim options:
    let b:useroptions = BackupOptions()

    " set larlet's Vim options:
    setl scrolloff=1
    setl list lcs=tab:»\ ,trail:_ nowrap
    if &ch<2
        " ^ hmm, be cute only the first time
        setl ch=2
    endif
    let b:larlet_options_set = 1

    " backup larlet's Vim options:
    let b:larletoptions = BackupOptions()
endif


" restore user's Vim options / reset larlet's Vim options:
" (and change menu items)
fun! <SID>ToggleVimOptions()
    if !exists("b:larlet_options_set")
        " buffer deleted, etc.
        return
    endif
    if b:larlet_options_set
        " restore user's options:
        exec "setl ".b:useroptions
        let b:larlet_options_set = 0

        aunmenu Larlet.Restore\ my\ options
        amenu 500.540 Larlet.Reset\ Larlet\ options  :call <SID>ToggleVimOptions()<cr>
        exec 'tmenu Larlet.Reset\ Larlet\ options  :setl'.b:larletoptions
    else
        " reset larlet's options:
        " again, backup user's current options:
        let b:useroptions = BackupOptions()
        exec "setl ".b:larletoptions
        let b:larlet_options_set = 1

        aunmenu Larlet.Reset\ Larlet\ options
        amenu 500.540 Larlet.Restore\ my\ options  :call <SID>ToggleVimOptions()<cr>
        exec 'tmenu Larlet.Restore\ my\ options  :setl'.b:useroptions
    endif
endf

" add menu items (also when resourcing):
silent! aunmenu Larlet
amenu <silent> 500.10 &Larlet.Toggle\ &key\ binding\	F8  :call <SID>ToggleKeyBinding()<cr>
amenu <silent> 500.20 Larlet.Set\ Space&Len  :call <SID>InputSpaceLen()<cr>
tmenu Larlet.Set\ SpaceLen  Number of space characters between symbols
amenu <silent> 500.30 Larlet.Set\ Space&Char  :call <SID>InputSpaceChar()<cr>
tmenu Larlet.Set\ SpaceChar  String to take space characters from
amenu <silent> 500.40 Larlet.Set\ E&xtraSpaces  :call <SID>InputExtraSpaces()<cr>
tmenu Larlet.Set\ ExtraSpaces  Extends symbol spacing when Spacebar is pressed
amenu Larlet.-line-  :
if !exists("b:larlet_options_set") || b:larlet_options_set
    amenu 500.540 Larlet.Restore\ my\ options  :call <SID>ToggleVimOptions()<cr>
    exec 'tmenu Larlet.Restore\ my\ options  :setl'.b:useroptions
else
    amenu 500.540 Larlet.Reset\ Larlet\ options  :call <SID>ToggleVimOptions()<cr>
    exec 'tmenu Larlet.Reset\ Larlet\ options  :setl'.b:larletoptions
endif
" how to define buffer-local menus?
amenu 500.550 Larlet.Remove\ this\ menu  :aunmenu Larlet<cr>

fun! <SID>InputSpaceLen()
    if !exists("b:SpaceLen")
        " buffer deleted, etc.
        return
    endif
    let nsl = input("SpaceLen=", b:SpaceLen)
    if nsl!=""
        call SetSpaceLen(nsl)
    endif
endf

fun! <SID>InputSpaceChar()
    if !exists("b:SpaceChar")
        " buffer deleted, etc.
        return
    endif
    let nsc = input("SpaceChar=", b:SpaceChar)
    if nsc!=""
        call SetSpaceChar(nsc)
    endif
endf

fun! <SID>InputExtraSpaces()
    if !exists("b:ExtraSpaces")
        " buffer deleted, etc.
        return
    endif
    let nes = input("ExtraSpaces=", b:ExtraSpaces)
    if nes!=""
        call SetExtraSpaces(nes)
    endif
endf

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Initial Values:

" the chicken and egg problem:
let b:ExtraSpaces = 1

" number of space characters to be put before a symbol:
call SetSpaceLen(1)

" string to take space characters from, should not contain characters that
" symbols consist of (" " is an exception):
call SetSpaceChar(" ")

" fixed number of extra spaces if space-key pressed (see wordspace), esp.
" useful when SpaceLen is zero:
call SetExtraSpaces(b:ExtraSpaces)
" (redundant, but keep it nevertheless)

" current number of preserved space characters:
let s:prespace = 0


" vim: set tw=0 ts=4 et nowrap :
