" File: leBrief.vim
" Purpose: Basic Brief Like emulation for vim
" Author: Lechee.Lai
" Version: 1.0
"
" The script is not a brief emulation just a clone for brief like 
" for beginner whom familiar with brief style editing with Meta-xx
" This script is only guide for non-vim user use vim
" 
" leBrief.vim base on Yegappan iakshmanan brief.vim
" 
" brief http://www.vim.org/scripts/script.php?script_id=265
" 
" Most good script accompany with Brief like emulation
" bufexplorer.vim http://www.vim.org/scripts/script.php?script_id=42
" sessions.vim http://www.vim.org/tips/tip.php?tip_id=450
" mru.vim http://www.vim.org/scripts/script.php?script_id=521
"
"
"

set nocompatible
set laststatus=2
set backspace=indent,start
set whichwrap=b,s,<,>,[,]
set nowrap
set sidescroll=1
set wildmenu
set ruler
set nowrapscan
set virtualedit=all
set winaltkeys=no
" set confirm
set nostartofline
set noincsearch		

" ---------- B r i e f E X  l i k e ------------- 

" Next Buffer
inoremap <silent> <A-.> <C-O>:bnext!<CR>
map <silent> <A-.> :bnext!<CR>

" Pervious Buffer
inoremap <silent> <A-,> <C-O>:bprevious!<CR>
map <silent> <A-,> :bprevious!<CR>

" Close
inoremap <silent> <A-x> <C-O>:bdelete<CR>
map <silent> <A-x> :bdelete<CR>

" Write
inoremap <silent> <A-w> <C-O>:w<CR>
map <silent> <A-w> :w<CR>

" Quit
inoremap <silent> <A-q> <C-O>:confirm quit<CR>
map <silent> <A-q> :confirm quit<CR>

" Undo
inoremap <silent> <A-u> <C-O>u
                             
" Redo                              
inoremap <silent> <C-U> <C-O>:redo<CR>

" Delete to End of Line
inoremap <silent> <A-k> <C-O>d$
map <silent> <A-k> d$

" Delete Line
inoremap <silent> <A-d> <C-O>dd
map <silent> <A-d> dd

" Buffer List (better use bufExplorer.vim) 
"inoremap <silent> <A-b> <C-O>:buffers<CR>:buffer
"map <silent> <A-b> :buffers<CR>:buffer

" Edit 
inoremap <A-e> <C-O>:edit 
map <A-e> :edit 

" Search
"inoremap <A-s> <C-O>:call <SID>SearchWord(expand("<cword>"))<CR>
inoremap <A-s> <C-O>/<C-R><C-W>
inoremap <F5> <C-O>/
map <F5> /
map <A-s> /<C-R><C-W>

" Search Next 
inoremap <A-]> <C-O>n
map <A-]> n

" Search Pervious
inoremap <A-[> <C-O>N
map <A-[> N

" Auto Complete                    
inoremap <silent> <A-/> <C-p>

" Match Brace
inoremap <silent> <F7> <C-O>%
map <silent> <F7> %


"-----------------------
" Windows
"-----------------------

" Split window
inoremap <silent> <F3> <C-O>:split<CR>
map <silent> <F3> :split<CR>

" Delete window
inoremap <silent> <F4> <C-O>:quit<CR>
map <silent> <F4> :quit<CR>

" Switch Window
inoremap <silent> <F6> <C-O><C-W>w 
map <silent> <F6> <C-W>w
                       
"-----------------------
" Bookmark
"-----------------------

" Mark bookmark 1
inoremap <silent> <A-1> <C-O>mc
map <silent> <A-1> mc

" Mark bookmark 2
inoremap <silent> <A-2> <C-O>md
map <silent> <A-2> md

" Mark bookmark 3
inoremap <silent> <A-3> <C-O>me
map <silent> <A-3> me

" Mark bookmark 4
inoremap <silent> <A-4> <C-O>mf
map <silent> <A-4> mf

" Mark bookmark 5
inoremap <silent> <A-5> <C-O>mg
map <silent> <A-5> mg

" Mark bookmark 6
inoremap <silent> <A-6> <C-O>mh
map <silent> <A-6> mh

" Mark bookmark 7
inoremap <silent> <A-7> <C-O>mi
map <silent> <A-7> mi

" Mark bookmark 8
inoremap <silent> <A-8> <C-O>mj
map <silent> <A-8> mj

" Mark bookmark 9
inoremap <silent> <A-9> <C-O>mk
map <silent> <A-9> mk

" Jump to a bookmark
inoremap <silent> <A-j> <C-O>:call <SID>BriefJumpMark()<CR>
map <silent> <A-j> :call <SID>BriefJumpMark()<CR>

"
" brief-home-key
"inoremap <silent> <Home> <C-O>:call <SID>BriefHomeKey()<CR>
"map <silent> <Home> :call <SID>BriefHomeKey()<CR>

" brief-end-key
"inoremap <silent> <End> <C-O>:call <SID>BriefEndKey()<CR>
"map <silent> <End> :call <SID>BriefEndKey()<CR>

" Toggle insert mode
inoremap <silent> <A-i> <Ins>

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
inoremap <silent> <A-y> <C-O>#*
map <silent> <A-y> #*
nmap = viw"ay

" Copy line or mark to scrap buffer.  Vim register 'a' is used as the scrap
" buffer
inoremap <silent> <kPlus> <C-O>"ayy
vnoremap <silent> <kPlus> "ay

" Cut line or mark to scrap buffer.  Vim register 'a' is used as the scrap
" buffer
inoremap <silent> <kMinus> <C-O>"add
vnoremap <silent> <kMinus> "ax

" Paste scrab buffer contents to current cursor position.  Vim register 'a' is
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

" Goto line
inoremap <silent> <A-g> <C-O>:call <SID>BriefGotoLine()<CR>
map <silent> <A-g> :call <SID>BriefGotoLine()<CR>

"-----------------------
" Misc commands
"-----------------------

" Start shell
inoremap <silent> <A-z> <C-O>:stop<CR>
map <silent> <A-z> :stop<CR>

"inoremap <A-h> <C-O>:call <SID>BriefHelp(expand("<cword>"))<CR>
inoremap <A-h> <C-O>K
map <A-h> K

"------ M i S C ----------------------
inoremap <F10> <C-O>:
inoremap <F8> <C-O>[I:let nr= input("Which one: ")<Bar> exe "normal " . nr . "[\t"<CR>
map <F8> [I:let nr = input("Which one: ")<Bar> exe "normal " . nr . "[\t"<CR>
inoremap <A-v> <C-O>:call <SID>BriefOccour()<CR>
map <A-v> :call <SID>BriefOccour()<CR>

" Tags pattern for selection
inoremap <C-F> <C-O>:tjump <C-R><C-W><CR>
map <C-F> :tjump <C-R><C-W><CR>

" Tag back from :pop/tag
inoremap <C-B> <C-O>:pop<CR>
map <C-B> :pop<CR>
              
"-----------------------------------------------------------------------
function! s:BriefOccour()
   let pattern = input("Prompt Find: ")
   exe "ilist /" . pattern
   let nr = input("Which one: ")
   " have problem goto specify line
   "   exe "normal " . nr . "[\t"
endfunction


function! s:BriefHomeKey()
   " if we are on the first char of the line, go to the top of the screen
   if (col(".") <= 1)
      " if on top of screen, go to top of file
      let l:a = line(".")
      normal H0
      if (line(".") == l:a)
         " we did not move! so ...
         normal 1G
      endif
   else
      " goto beginning of line
      normal 0
   endif
endfunction
       
" This function is taken from vim online web page and modified
function! s:BriefEndKey()
    let cur_col = virtcol(".")
    let line_len = virtcol("$")

    if cur_col != line_len
        " The cursor is not at the end of the line, goto the end of the line
        execute "normal " . line_len . "|"
    else
        " The cursor is already at the end of the line
        let cur_line = line(".")
        normal L         " goto the end of the current page
        if line(".") == cur_line
            " Cursor is already at the end of the page
            normal G  " Goto the end of the file
            let line_len = virtcol("$")
            if line_len > 0
                execute "normal " . line_len . "|"
            endif
        else
            " Cursor was not already in the end of the page
            let line_len = virtcol("$")
            if line_len > 0
                execute "normal " . line_len . "|"
            endif
        endif
    endif
endfunction

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
       
function! s:BriefJumpMark()
    let mark = input("Jump to bookmark: ")
    if mark == ""
        return
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

function! s:BriefGotoLine()
    let linenr = input("Line number to jump to: ")
    if linenr == ""
        return
    endif

    execute "normal " . linenr . "gg"
endfunction

