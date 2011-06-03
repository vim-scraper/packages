" File: brief2.vim
" Authors: Richard Jones, Yegappan Lakshmanan
" Derived From: brief.vim by Yegappan Lakshmanan
" Version: 1.01
" Last Modified: 2007-02-23
"
" Since I couldn't update Yegappan's script, I just created a new project
" called "brief2", but it's based on Yegappan's work.
"
" Changes:
" 2005-04-05 Richard Jones
"    -Improved BriefHome and BriefEnd by using states (better, but not exact)
"    -Improved Search & SearchAgain
"    -Added BriefDelete to provide better 'Del' key handling
"
" Overview
" --------
" The brief.vim Vim plugin emulates the brief editor key bindings and behavior
" in Vim.
"
" Note1: This plugin uses the Vim "insertmode".  To properly use this plugin,
" you have to be in insertmode always.  If you press escape, Vim will not
" go to the normal mode.  If you want to use any of the normal commands,
" press <C-O> and then the normal mode command.
"
" Note2: This plugin uses the CTRL and ALT keys for the mappings. Before
" using this plugin, make sure that the ALT and CTRL keys work in your Vim
" installation.
"
" The following list of brief like key-mappings are provided:
"
" Cursor Movement Keys
" --------------------
" <Up>         - Move cursor up one line
" <Down>       - Move cursor down one line
" <C-Left>     - Goto beginning of previous word
" <C-Right>    - Goto beginning of next word
" <Home>       - Home key.  If you press once, cursor will move to the first
"                column in the current line.  If you press twice, cursor will
"                move to the first column in the first line in the current
"                page.  If you press thrice, cursor will be positioned at the
"                top of the file.
" <End>        - End key. If you press once, cursor will move to the last
"                column in the current line.  If you press twice, cursor will
"                move to the last column in the last line in the current page.
"                If you press thrice, cursor will be positioned at the end of
"                the file.
" <C-PageUp>   - Goto-beginning of file
" <C-PageDown> - goto-end of file
" <C-Home>     - Beginning-of-window
" <C-End>      - End-of-window
" <C-d>        - Scroll line down
" <C-e>        - Scroll line up
" <A-Home>     - Move the cursor to the first character on screen
" <A-End>      - Move the cursor to the last character on screen
" <C-b>        - Move the current line to the bottom of the window
" <C-c>        - Move the current line to the center of the window
" <C-t>        - Move the current line to the top of the window
" 
" Editing Keys
" ------------
" <C-CR>       - Open a new line below the current line and goto that line
" <S-CR>       - open a new line below the current line, cursor stays in the
"                current line
" <A-i>        - Toggle insert mode
" <A-k>        - Delete from the cursor position to the end of line
" <C-k>        - Delete from the cursor position to the start of line
" <A-d>        - Delete the current line
" <kPlus>      - Copy line or mark to scrap buffer.  Vim register 'a' is used
"                as the scrap  buffer.
" <kMinus>     - Cut line or mark to scrap buffer.  Vim register 'a' is used
"                as the scrap buffer.
" <Ins>        - Paste scrap buffer contents to current cursor position.  Vim
"                register 'a' is used as the scrap buffer
" <C-Ins>      - Copy marked text to system clipboard.  If no mark, copy
"                current line
" <S-Ins>      - Paste the system clipboard contents to current cursor
" <S-Del>      - Cut the marked text to system clipboard. If no mark, cut the
"                current line
" <C-Del>      - Remove the marked text
" <C-v>        - Clipboard paste
" <A-g>        - Goto line
" <C-BS>       - Delete the previous word
" <A-BS>       - Delete the next word
" <A-/>        - Complete a partially typed word
" <A-q>        - Quote the next character
" 
" Delete Keys
" -----------
" <A-u>        - 
" <kMultiply>  - Undo last operation.  Either keypad * key or <A-u> can be
"                used.
" <A-y>        - Restore line
" <C-y>        - Redo the previously undid commands
" 
" Search and Replace Commands
" ---------------------------
" <C-f>        -
" <F5>         -
" <A-s>        - String search
" <S-F5>       - search again
" <A-F5>       - Reverse search
" <F6>         - Search and replace from the current cursor position
" <A-t>        - Search and replace the current word from the current cursor
"                position
" <S-F6>       - Repeat last search and replace
" <C-F5>       - toggle case sensitivity of search commands.
" 
" Buffer Commands
" ---------------
" <A-e>        - Open file
" <A-x>        - Exit
" <A-r>        - Read file
" <A-w>        - Save the current file
" <A-o>        - Save the current file in a different file name
" <A-n>        - Select next buffer from the buffer list
" <A-->        - Select previous buffer from the buffer list
" <A-p>        - Select previous buffer from the buffer list
" <C-->        -
" <C-kMinus>   - Delete current buffer from buffer list
" <A-b>        - Display buffer list
" <A-f>        - Display buffer information
" 
" Compiler related commands
" -------------------------
" <A-F10>      - Compile current buffer
" <C-n>        - Jump to the next error
" <C-l>        - pJump to the next previous error
" <C-p>        - View compiler output
" 
" Mark commands
" -------------
" <A-m>        - Toggle standard text marking mode
" <A-l>        - Toggle line marking mode
" <A-c>        - 
" <A-a>        - Toggle column marking mode
" <A-h>        - Mark current word
" 
" Misc commands
" -------------
" <A-v>        - Show version
" <C-Up>
" <C-Down>     - Goto next/previous function
" <A-z>        - Start a shell
" <A-F1>       - Search for a keyword in online help
" 
" Bookmark
" --------
" <A-0>        - Mark bookmark 0
" <A-1>        - Mark bookmark 1
" <A-2>        - Mark bookmark 2
" <A-3>        - Mark bookmark 3
" <A-4>        - Mark bookmark 4
" <A-5>        - Mark bookmark 5
" <A-6>        - Mark bookmark 6
" <A-7>        - Mark bookmark 7
" <A-8>        - Mark bookmark 8
" <A-9>        - Mark bookmark 9
" <A-j>        - Jump to a bookmark
" 
" Windows Commands
" ----------------
" <F3>         - Split window
" <F4>         - Delete window
" <A-F2>       - Zoom window
" <A-Down>     - Goto the window below the current window
" <A-Up>       - Goto the window above the current window
"

" Has this already been loaded?
if exists("loaded_brief")
  finish
endif
let loaded_brief=1

" Variable Initialization

let g:searchstr   = ""
let g:home_state  = 1
let g:end_state   = 1
let g:cursor_col  = wincol()
let g:cursor_row  = winline()
let g:cursor_line = line(".")

"
" The following Vim settings are used for emulating brief like behavior
"
" Enable Vim features
set nocompatible

" Confirm certain operations
set confirm

" Always have a status line
set laststatus=2

" Only insert mode is supported
set insertmode

" Allow backspace over everything
set backspace=indent,start

" Wrap to the line above the current one when using arrow keys, backpsace
" and space character
" set whichwrap=b,s,<,>,[,]

" Do not wrap long lines
set nowrap

" Scroll only one column when typing text wider than the screen
set sidescroll=1

" Incremental search
set incsearch

" Bring a list of filenames when <tab> is pressed
set wildmenu

" Always display the current line and column
set ruler

" Wrap while searching for patterns
set wrapscan

"set hidden

" Allow editing in out-of-text area
set virtualedit=all

" Keep the cursor in the same column for some of the cursor movement commands
set nostartofline

" Disable menu accelerators.  The Alt key that activates the menu interfere
" with the Brief key mappings.
set winaltkeys=no

"-----------------------
" Cursor movement
"-----------------------

" Make tab key handle indent of marked text
" (NOT WORKING YET)
"inoremap <silent> <Tab> <C-O>:call <SID>BriefTabKey()<CR>

" Make cursor keys ignore wrapping
inoremap <silent> <Down> <C-O>gj
inoremap <silent> <Up> <C-O>gk

" goto beginning of previous word
inoremap <silent> <C-Left> <C-O>B

" goto beginning of next word
inoremap <silent> <C-Right> <C-O>W

" brief-home-key
inoremap <silent> <Home> <C-O>:call <SID>BriefHomeKey()<CR>

" brief-end-key
inoremap <silent> <End> <C-O>:call <SID>BriefEndKey()<CR>

" goto-beginning of file
inoremap <silent> <C-PageUp> <C-O>gg

" goto-end of file
inoremap <silent> <C-PageDown> <C-O>G

" beginning-of-window
inoremap <silent> <C-Home> <C-O>H

" end-of-window
inoremap <silent> <C-End> <C-O>L

" scroll line down
inoremap <silent> <C-d> <C-x><C-y>

" scroll line up
inoremap <silent> <C-e> <C-x><C-e>

" Move the cursor to the first character on screen
inoremap <silent> <A-Home> <C-O>g0

" Move the cursor to the last character on screen
inoremap <silent> <A-End> <C-O>g$

" Move the current line to the bottom of the window
inoremap <silent> <C-b> <C-O>z-

" Move the current line to the center of the window
inoremap <silent> <C-c> <C-O>z.

" Move the current line to the top of the window
inoremap <silent> <C-t> <C-O>z<CR>

"-----------------------
" Editing
"-----------------------

" Open a new line below the current line and goto that line
inoremap <silent> <C-CR> <C-O>o

" open a new line below the current line, cursor stays in the current line
inoremap <silent> <S-CR> <C-O>o<C-O>k

" Toggle insert mode
inoremap <silent> <A-i> <Ins>

" Delete from the cursor position to the end of line
inoremap <silent> <A-k> <C-O>d$

" Delete from the cursor position to the start of line
inoremap <silent> <C-k> <C-U>

" Delete the current line
inoremap <silent> <A-d> <C-O>dd

" Copy line or mark to scrap buffer.  Vim register 'a' is used as the scrap
" buffer
inoremap <silent> <kPlus> <C-O>"ayy
vnoremap <silent> <kPlus> "ay

" Cut line or mark to scrap buffer.  Vim register 'a' is used as the scrap
" buffer
inoremap <silent> <kMinus> <C-O>"add
vnoremap <silent> <kMinus> "ax

" Paste scrap buffer contents to current cursor position.  Vim register 'a' is
" used as the scrap buffer
inoremap <silent> <Ins> <C-O>"aP

" Copy marked text to system clipboard.  If no mark, copy current line
inoremap <silent> <C-Ins> <C-O>"*yy
vnoremap <silent> <C-Ins> "*y

" Paste the system clipboard contents to current cursor
inoremap <silent> <S-Ins> <C-O>"*P

" Cut the marked text to system clipboard. If no mark, cut the current line
inoremap <silent> <S-Del> <C-O>"*dd
vnoremap <silent> <S-Del> "*d

" Remove the marked text
vnoremap <silent> <C-Del> <C-O>d

" Clipboard paste
inoremap <silent> <C-v> <C-O>"*P

" Goto line
inoremap <silent> <A-g> <C-O>:call <SID>BriefGotoLine()<CR>

" Delete the previous word
inoremap <silent> <C-BS> <C-W>

" Delete the next word
inoremap <silent> <A-BS> <C-O>E<C-O>E<C-O>a<C-W>

" Complete a partially typed word
inoremap <silent> <A-/> <C-p>

" Quote the next character
inoremap <silent> <A-q> <C-v>

"-----------------------
" Delete
"-----------------------

" Delete character to right of cursor
"inoremap <silent> <Del> <C-O>x
inoremap <silent> <Del> <C-O>:call <SID>BriefDelete()<CR>

" undo last operation.  Either keypad * key or <A-u> can be used
inoremap <silent> <A-u> <C-O>u
inoremap <silent> <kMultiply> <C-O>u

" Restore line
inoremap <silent> <A-y> <C-O>U

" Redo the previously undid commands
inoremap <silent> <C-y> <C-r>

"-----------------------
" Search and replace
"-----------------------

" string-search
inoremap <silent> <C-f> <C-O>:call <SID>BriefSearch("")<CR>
inoremap <silent> <F5> <C-O>:call <SID>BriefSearch("")<CR>

"inoremap <silent> <A-s> <C-O>:call <SID>BriefSearch(expand("<cword>")<CR>
inoremap <silent> <A-s> <C-O>:call <SID>BriefSearch("")<CR>

" search again
"inoremap <silent> <S-F5> <C-O>n
inoremap <silent> <S-F5> <C-O>:call <SID>BriefSearchAgain("")<CR>

" Reverse search
inoremap <silent> <A-F5> <C-O>N

" Search and replace from the current cursor position
inoremap <silent> <F6> <C-O>:call <SID>BriefSearchAndReplace("")<CR>

" Search and replace the current word from the current cursor position
inoremap <silent> <A-t> <C-O>:call <SID>BriefSearchAndReplace(expand("<cword>"))<CR>

" Repeat last search and replace
inoremap <silent> <S-F6> <C-O>:.,$&&<CR>

" toggle case sensitivity of search commands.
inoremap <silent> <C-F5> <C-O>:set invignorecase<CR>

"-----------------------
" Buffer
"-----------------------

" open file
inoremap <A-e> <C-O>:edit 

" exit
inoremap <silent> <A-x> <C-O>:confirm quit<CR>

" read file
inoremap <A-r> <C-O>:read  

" Save the current file
inoremap <silent> <A-w> <C-O>:call <SID>BriefSave()<CR>

" Save the current file in a different file name
inoremap <silent> <A-o> <C-O>:call <SID>BriefSaveAs()<CR>

" Select next buffer from the buffer list
inoremap <silent> <A-n> <C-O>:bnext<CR>

" Select previous buffer from the buffer list
inoremap <silent> <A--> <C-O>:bprevious<CR>
inoremap <silent> <A-p> <C-O>:bprevious<CR>

" Delete current buffer from buffer list
inoremap <silent> <C--> <C-O>:bdelete<CR>
inoremap <silent> <C-kMinus> <C-O>:bdelete<CR>

" Display buffer list
inoremap <A-b> <C-O>:buffers<CR>:buffer 

" Display buffer information
inoremap <A-f> <C-O>:file<CR>

"-----------------------
" Compiler related
"-----------------------
" compile-buffer
inoremap <silent> <A-F10> <C-O>:make<CR>

" next-error
inoremap <silent> <C-n> <C-O>:cnext<CR>

" previous error
inoremap <silent> <C-l> <C-O>:cprevious<CR>

" View compiler output
inoremap <silent> <C-p> <C-O>:copen<CR>

"-----------------------
" Mark
"-----------------------

" toggle standard text marking mode
inoremap <silent> <A-m> <C-O>v
vnoremap <silent> <A-m> v

" Toggle line marking mode
inoremap <silent> <A-l> <C-O>V
vnoremap <silent> <A-l> V

" Toggle column marking mode
inoremap <silent> <A-c> <C-O><C-V>
inoremap <silent> <A-a> <C-O><C-V>
vnoremap <silent> <A-c> <C-V>
vnoremap <silent> <A-a> <C-V>

" Mark current word
inoremap <silent> <A-h> <C-O>viw

"-----------------------
" Misc commands
"-----------------------

" show version
inoremap <silent> <A-v> <C-O>:version<CR>

" Goto next/previous function
inoremap <silent> <C-Up>    <C-O>[[
inoremap <silent> <C-Down>  <C-O>]]

" Start shell
inoremap <silent> <A-z> <C-O>:stop<CR>

" Search for a keyword in the online help
inoremap <A-F1> <C-O>:help  

"-----------------------
" Bookmark
"-----------------------

" Mark bookmark 0
inoremap <silent> <A-0> <C-O>mb

" Mark bookmark 1
inoremap <silent> <A-1> <C-O>mc

" Mark bookmark 2
inoremap <silent> <A-2> <C-O>md

" Mark bookmark 3
inoremap <silent> <A-3> <C-O>me

" Mark bookmark 4
inoremap <silent> <A-4> <C-O>mf

" Mark bookmark 5
inoremap <silent> <A-5> <C-O>mg

" Mark bookmark 6
inoremap <silent> <A-6> <C-O>mh

" Mark bookmark 7
inoremap <silent> <A-7> <C-O>mi

" Mark bookmark 8
inoremap <silent> <A-8> <C-O>mj

" Mark bookmark 9
inoremap <silent> <A-9> <C-O>mk

" Jump to a bookmark
inoremap <silent> <A-j> <C-O>:call <SID>BriefJumpMark()<CR>

"-----------------------
" Windows
"-----------------------
   
" Split window
inoremap <silent> <F3> <C-O>:split<CR>

" Delete window
inoremap <silent> <F4> <C-O>:quit<CR>

" Zoom window
inoremap <silent> <A-F2> <C-O>:only<CR>

" Goto the window below the current window
inoremap <silent> <A-Down> <C-O><C-W>w

" Goto the window above the current window
inoremap <silent> <A-Up> <C-O><C-W>W

"-----------------------------------------------------------------------
"
" Functions
"
"-----------------------------------------------------------------------

"-----------------------------------------------------------------------
" Insert text directly into document
"-----------------------------------------------------------------------
function! s:InsertText(txt)
   let output = a:txt
   execute "normal i" . output
   let temp = cursor( 0, wincol()+1 )
endfunction

"-----------------------------------------------------------------------
" Perform TAB operation while considering visual marking
" FIXME: This doesn't work
"-----------------------------------------------------------------------
function! s:BriefTabKey()
   call s:InsertText("\<Tab>")
endfunction

"-----------------------------------------------------------------------
" Delete a character and account for joining lines if at end of line
"-----------------------------------------------------------------------
function! s:BriefDelete()
   if( wincol() == virtcol("$") )
      "do nothing for now
      normal J
   else
      normal x
   endif
endfunction

"-----------------------------------------------------------------------
" Properly process Home key operation
"-----------------------------------------------------------------------
function! s:BriefHomeKey()
   let g:end_state = 1    "this func affects BriefEndKey()
   let ccol  = wincol()
   let crow  = winline()
   let cline = line(".")
   if( (ccol == g:cursor_col) && (crow == g:cursor_row) && (cline == g:cursor_line) )
      if( g:home_state == 1 )
         normal 0
         let g:home_state = 2
      elseif( g:home_state == 2 )
         normal H0
         let g:home_state = 3
      elseif( g:home_state == 3 )
         normal 1G
         let g:home_state = 1
      endif
   else
      normal 0
      let g:home_state = 2
   endif
   " save for next time comparison
   let g:cursor_col  = wincol()
   let g:cursor_row  = winline()
   let g:cursor_line = line(".")
endfunction

"-----------------------------------------------------------------------
" Properly process End key operation
"-----------------------------------------------------------------------
function! s:BriefEndKey()
   let g:home_state = 1   "this func affects BriefHomeKey()
   let ccol  = wincol()
   let crow  = winline()
   let cline = line(".")
   if( (ccol == g:cursor_col) && (crow == g:cursor_row) && (cline == g:cursor_line) )
      if( g:end_state == 1 )
         " goto end of line
         let g:end_state = 2
      elseif( g:end_state == 2 )
         " goto end of page
         normal L
         let g:end_state = 3
      elseif( g:end_state == 3 )
         " goto end of file
         normal G
         let g:end_state = 1
      endif
   else
      " goto end of line
      let g:end_state = 2
   endif
   " goto end of line: line length + 1
   let temp = cursor( 0, virtcol("$") )
   " save for next time comparison
   let g:cursor_col  = wincol()
   let g:cursor_row  = winline()
:   let g:cursor_line = line(".")
endfunction

"-----------------------------------------------------------------------
" Search for a string
"-----------------------------------------------------------------------
function! s:BriefSearch(pattern)
    if has("gui_running")
        if a:pattern == ""
            promptfind
        else
            execute "promptfind " . a:pattern
        endif
    else
        let g:searchstr = input("Find text: ", a:pattern)
        if g:searchstr == ""
            return
        endif
        execute '/' . g:searchstr
    endif
endfunction

"-----------------------------------------------------------------------
" Repeat search operation
"-----------------------------------------------------------------------
function! s:BriefSearchAgain(pattern)
    execute '/' . g:searchstr
endfunction

"-----------------------------------------------------------------------
" Search and Replace (Translate)
"-----------------------------------------------------------------------
function! s:BriefSearchAndReplace(pattern)
    if has("gui_running")
        if a:pattern == ""
            execute "promptrepl " . a:pattern
        else
            promptrepl
        endif
    else
        let g:searchstr = input("Find text: ", a:pattern)
        if g:searchstr == ""
            return
        endif

        let replacestr = input("Replace with: ")
        if replacestr == ""
            return
        endif
        execute '.,$substitute/' . g:searchstr . '/' . replacestr . '/c'
    endif
endfunction

"-----------------------------------------------------------------------
" Save File
"-----------------------------------------------------------------------
function! s:BriefSave()
    if expand("%") == ""
        if has("gui_running")
            browse write
        else
            let fname = input("Save file as: ")
            if fname == ""
                return
            endif
            execute "write " . fname
        endif
    else
        write!
    endif
endfunction

"-----------------------------------------------------------------------
" Save File As
"-----------------------------------------------------------------------
function! s:BriefSaveAs()
    if has("gui_running")
        browse saveas
    else
        let fname = input("Save file as: ")
        if fname == ""
            return
        endif
        execute "saveas " . fname
    endif
endfunction

"-----------------------------------------------------------------------
" Jump to a Bookmark
"-----------------------------------------------------------------------
function! s:BriefJumpMark()
    let mark = input("Jump to bookmark: ")
    if mark == ""
        return
    endif
    if mark == "0"
        normal `b
    endif
    if mark == "1"
        normal `c
    endif
    if mark == "2"
        normal `d
    endif
    if mark == "3"
        normal `e
    endif
    if mark == "4"
        normal `f
    endif
    if mark == "5"
        normal `g
    endif
    if mark == "6"
        normal `h
    endif
    if mark == "7"
        normal `i
    endif
    if mark == "8"
        normal `j
    endif
    if mark == "9"
        normal `k
    endif
endfunction

"-----------------------------------------------------------------------
" Goto Selected Line
"-----------------------------------------------------------------------
function! s:BriefGotoLine()
    let linenr = input("Line number to jump to: ")
    if linenr == ""
        return
    endif
    execute "normal " . linenr . "gg"
endfunction

