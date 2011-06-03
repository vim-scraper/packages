" QuickASCII - Vim global plugin for easy adding of ASCII chars
" File: qascii.vim
" Last change: 2002 Nov 15
" Maintainer: Peter Valach <pvalach@gmx.net>
" Version: 2.0
" License: GPL 2
"
" Description:
"   Yet another ASCII table plugin :). I often need to insert some high-ASCII
"   character that my keymap doesn't allow to write and none of the *ASCII
"   plugins I saw was so easy and quick to use, so I wrote my own. It's my
"   first plugin for VIM and I welcome any bug reports, ideas and suggestions.
"
"   Since version 2.0 this plugin allows to use any file as ASCII table. This
"   way you can create your own file with chars you need and layout you feel
"   comfortable with and use it for input.
"
"   I tested this plugin only on Linux version of VIM 6.1, but it should work
"   in all VIM 6.x versions on all platforms.
"
" Usage:
"   Plugin creates two functions: QuickAscii and QuickAsciiFile.
"
"   * QuickAscii() *
"
"   Takes following parameters:
"     1:columns -- number of columns to show characters in
"     2:split   -- 0 for horizontal, 1 for vertical split (see below)
"      + any number of pairs of these:
"       first   -- number of first character to display
"       last    -- number of last character to display
"
"   If you use UTF8 encoding, you can freely use numbers over 255 for
"   displaying Unicode characters. Double-width characters are not handled
"   special, therefore a table containing them may not look good - use of
"   QuickAsciiFile() is recommended in this case.
"
"   For practical use it's recommended to create a mapping with your favourite
"   parameters:
"
"   Example 1 (64 columns, horizontal, all high-ASCII chars):
"   :nmap <f2> :call QuickAscii(64,0,128,255)<cr>
"   :imap <f2> <Esc>:call QuickAscii(64,0,128,255)<cr>
"
"   Example 2 (10 columns, vertical, number and upper-case characters):
"   :nmap <f2> :call QuickAscii(10,1,48,57,65,90)<cr>
"   :imap <f2> <Esc>:call QuickAscii(10,1,48,57,65,90)<cr>
"
"   * QuickAsciiFile() *
"
"   Takes exactly three parameters, all of the are required:
"       size     -- number of rows/columns for new window
"       split    -- 0 for horizontal, 1 for vertical split (see below)
"       filename -- name of file to load
"
"   Function loads given file and uses it's contents for input. It's cool if
"   you create a file with all characters you frequently need to insert (maybe
"   with help of QuickAscii() function), because you may arrange them as you
"   wish.
"
"   For practical use it's recommended to create a mapping with your favourite
"   parameters and file. For example:
"   :nmap <f3> :call QuickAscii(10,0,"~/.vim/data/my_asciitable.txt")<cr>
"   :imap <f3> <Esc>:call QuickAscii(10,0,"~/.vim/data/my_asciitable.txt")<cr>
"       
" Interface:
"   Plugin works by splitting of active window (horizontal or vertical). There
"   it displays given range(s) of ASCII chars or given file contents. You may
"   use all movement commands as usual, only these are redefined:
"       <i> will put char under cursor on cursor-position of previous window
"       <a> will put char under cursor after cursor-position of previous window
"       <Enter> will put character under cursor after cursor-position, close
"           plugin window and start insert mode
"       <Shift>-<Enter> will do the same as <Enter>, without starting insert
"       <q> and <Esc> will close plugin window
"
" Install:
"   Just put this file (qascii.vim) into your plugin directory or put it
"   anywhere and source it (replace {YOUR-PATH} with real path, of course :)
"   :source {YOUR-PATH}/qascii.vim
"
"   It's highly recommended to create some mappings afterwards (see Usage).
"
" Bugs:
"   After going back to vertical split window using <CTRL-W><p> in VIM 6.1 it
"   sometimes resizes automatically. I don't know if it's a bug or feature :)
"   and I'll appreciate any suggestions about this issue.
"
" History:
"   2.0 (2002/11/15)
"   - QuickAscii function now accepts any number of intervals to display
"   - new QuickAsciiFile function allows you to use any file for input
"   - added iskeyword so that word-movement commands skip everything but space
"   - cursor is placed on top of window
"   - code overworked a lot :)
"   1.0 (2002/07/06)
"   - lots of code cleanups
"   - made all commands silent
"   - added <Esc> and <Shift>-<Enter> mappings
"   - added split type (horizontal/vertical) option
"   - added 'first' and 'last' parameters
"   - 'first line empty' workaround
"   - updated <Enter> mapping to close window and start insert mode
"   - added all header texts
"   0.1 (2002/07/05)
"   - first working version (not released :)


" load this function only once
if exists("loaded_quickascii")
  finish
endif
let loaded_quickascii = 1

" just to be sure :)
let s:cpo_save = &cpo
set cpo&vim

function! s:qOpenWindow(size, splt) " {{{
    " if split variable is 1, split vertical, otherwise "normal" :)
    if (a:splt == 1)
        silent execute a:size.'vsplit __QuickASCII'
    else
        silent execute a:size.'split __QuickASCII'
    endif
    setlocal noswapfile
    setlocal buftype=nowrite
    setlocal bufhidden=delete
    setlocal nonumber
    setlocal nowrap
    setlocal norightleft
    setlocal foldcolumn=0
    setlocal iskeyword=1-31,32-255
    " it must be modifiable until content is written :)
    setlocal modifiable
endfunction " }}}

function! s:qPrepareWindow() " {{{
    " don't allow to modify contents
    setlocal nomodifiable
    " <i> will put character under cursor into text of previous window
    noremap <silent> <buffer> i "myl<c-w>pP<c-w>p
    " <a> will put character under cursor after text of previous window
    noremap <silent> <buffer> a "myl<c-w>pp<c-w>p
    " <Enter> will put character under cursor after text, close and insertmode
    noremap <silent> <buffer> <cr> "myl<c-w>pp<c-w>p<c-w>ca
    " <S-Enter> will put character under cursor after text and close
    noremap <silent> <buffer> <s-cr> "myl<c-w>pp<c-w>p<c-w>c
    " <q> and <Esc> will close window
    noremap <silent> <buffer> q <c-w>c
    noremap <silent> <buffer> <Esc> <c-w>c
    " go to start of file
    normal gg
endfunction " }}}

function! QuickAscii(...) " {{{
    " Desc:
    " 1:columns -- number of columns to show characters in
    " 2:split   -- 0 for horizontal, 1 for vertical split
    "  ... followed by any number of first-last pairs:
    "   first -- number of first character to display
    "   last  -- number of last character to display

    " * set variables {{{

    "cols -- columns (number of chars on one line)
    let cols = a:1
    let splt = a:2

    " * set variables }}}

    " * create new window {{{

    " if split variable is 1, split vertical, otherwise "normal" :)
    if (splt == 1)
        let size = cols
    else
        let size = 10
    endif

    call s:qOpenWindow(size, splt)

    " * create new window }}}

    " * print chars {{{
    
    "idx -- parameter counter
    let idx = 4
    "rowc -- row counter
    let rowc = 0
    "firstline -- nothing was printed yet
    let firstline = 1

    " go through all first-last pairs
    while idx <= a:0

        " get interval data {{{

        "ch1 -- char number to start with
        exe "let ch1 = a:" . (idx - 1)
        "ch2 -- char number to end with
        exe "let ch2 = a:" . idx

        " increase parameter counter
        let idx = idx + 2

        " if first is before last, exchange them
        if (ch1 > ch2)
            let i = ch1
            let ch1 = ch2
            let ch2 = i
        endif

        " get interval data }}}

        " do some calculations {{{

        "chnum -- number of chars
        let chnum = ch2 - ch1 + 1
        "rows -- rows
        let rows = chnum / cols
        " if not exact, add one row
        if (chnum % cols != 0)
            let rows = rows + 1
        endif
        " update row counter
        let rowc = rowc + rows

        " do some calculations }}}

        " print one interval {{{

        let i = 0

        " ascii table is printed in rows
        while(i < rows)

            let j = 0

            "out -- string of one line
            let out = ""  

            " print 64 chars on line
            while((j < cols) && ((ch1 + j + i * cols) <= ch2))
                let out = out.nr2char(ch1 + j + i * cols)
                " next char
                let j = j + 1
            endwhile

            " print line (first line with !, so there's no first empty line :)
            if (firstline == 1)
                let firstline = 0
                silent put! =out
            else
                silent put =out
            endif

            " next line
            let i = i + 1
        endwhile

        " print one interval }}}

    endwhile

    " * print chars }}}

    " * post-display buffer settings {{{

    " set win size to total number of rows
    if(splt == 0)
        execute "resize ".rowc
    endif

    " all window settings and maps
    call s:qPrepareWindow()

    " * post-display buffer settings }}}

endfunction " }}}

function! QuickAsciiFile(size, splt, fn) " {{{
    " Desc:
    " size -- number of rows/columns for new window
    " splt -- 0 for horizontal, 1 for vertical split
    " fn   -- name of file to load

    " create new window
    call s:qOpenWindow(a:size, a:splt)

    " load file
    silent execute "0r ".a:fn

    " all window settings and maps
    call s:qPrepareWindow()

endfunction " }}}

" restore 'cpo'
let &cpo = s:cpo_save
