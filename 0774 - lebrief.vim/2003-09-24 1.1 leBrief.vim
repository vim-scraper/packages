" File: leBrief.vim
" Purpose: Basic Brief Like emulation for vim
" Author: Lechee.Lai
" Version: 1.1
"
" I'm also a freshmen with vim (in one week) since some behaviour 
" can not familiar in vim for example switch the buffer need type 
" :bn(ext) / :bp(revious) it take a long time for me so my 
" goal is define most recent use function as Meta-XX as brief 
" like (not All Meta key combine are available in insert mode)  
"
" The script is not a brief emulation just a clone for brief like 
" for beginner whom familiar with brief style editing with Meta-xx
" This script is only guide for non-vim user use vim with A little 
" brief style and explore the power of vim mode
" 
" === so make you own vim as start from here ===
" 
" leBrief.vim base on Yegappan iakshmanan brief.vim
" 
" brief.vim http://www.vim.org/scripts/script.php?script_id=265
" 
" Most good script accompany with Brief like emulation
" bufexplorer.vim http://www.vim.org/scripts/script.php?script_id=42
" sessions.vim http://www.vim.org/tips/tip.php?tip_id=450
" mru.vim http://www.vim.org/scripts/script.php?script_id=521
"
"
" 1.0  21 SEP 2003
"   Initial Revision
" 1.1  25 SEP 2003
"   Add more comment and a little hint
"   
"
" ==== Read first ==== and redefine yourself
"
" Keymap -- META                -- real command in vim -- 
" A-a   Empty                  
" A-b   Show buffers 		:ls :buffers :files
" A-c   Mark Column 
" A-d   Delete Line     	dd
" A-e   open file		:edit
" A-f 	Empty
" A-g   Goto Line		[line]gg
" A-h   Quick help      	K
" A-i   Insert/Replace
" A-j	Goto BookMark		`[a-z]
" A-k   Delete to End of Line   d$
" A-l	Mark Line               V
" A-m   Mark Stream             v
" A-n   Empty
" A-o   Empty
" A-p	Empty
" A-q	Quit			:q
" A-r	Read File               :read 
" A-s	Search                  /
" A-t   Replace                 %s/\<pattern\>/gci
"                               g = globe 
"                               c = confirm      
"                               i = ignore case
" A-u	undo			u
" A-v	find occurences         [Ctrl-I
" A-w	Write                   :w
" A-x	close			:close
" A-y   mark word for search	#*
" A-z	Shell			:stop
" A-,	Previous Buffer         :bprevious
" A-.   Next Buffer             :bnext
" A-[   Search Pervious         n
" A-]   Search Next             N
" A-/	Complete                C-P/C-N
" A-1..9 Bookmark Anchor        m[b..k]
" --CONTROL--
" C-B	tags back		:pop 
" C-F	find tags		:tjump
"
" F3    split window            :split
" F4    close window            :quit
" F5    search                  /
" F6    next window
" F7    Brace Match             %
" F8    Occurences under cursor [Ctrl-I
" F10   command mode
"                                
" ------------- tip keymap -------------------
" 
" <C-X><C-L> 	Complete for line          
" <C-X><C-F>    filename complete
" <C-P><C-N>    Word Complete Previous/Next
" g<C-]>	jump tag selection with duplicate case
" *		next locate for search under cursor 
" #             previous locate for search under cursor
" %		find match for ({[]})
" dd            delete line
" dw            delete word
" d$            delete to end of line
" d^            delete to begin of line
" J             join line
" u		undo
" /		search
" . 		repeat last command 
"
" more tip are in online vimtip. you will falling love with vim :-)
"
" ------------- tip for install --------------
"
"   most people like me can not use META key in Windows with East Asia Language
" install, I don't know why, but you can compiled the normal version to fixed
" the problem 
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

" Phoenix Atags compitable env(Tagfile)
if $tagfile != ""
   let &tags = $tagfile . '\tags'
endif
 
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
inoremap <silent> <A-q> <C-O>:confirm qa<CR>
map <silent> <A-q> :confirm qa<CR>

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

" read file
inoremap <A-r> <C-O>:read  
map <A-r> :read 

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

" replace
inoremap <A-t> <C-O>:%s/\<<c-r><c-w>\>
inoremap <F6> <C-O>:/%s/\<<c-r><c-w>\>

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



" ------ real brief home/end -------------------------------
"
" brief-home-key
"inoremap <silent> <Home> <C-O>:call <SID>BriefHomeKey()<CR>
"map <silent> <Home> :call <SID>BriefHomeKey()<CR>
"
" brief-end-key
"inoremap <silent> <End> <C-O>:call <SID>BriefEndKey()<CR>
"map <silent> <End> :call <SID>BriefEndKey()<CR>
"
" ------ real brief home/end -------------------------------
"
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

" Mark current word for search
inoremap <silent> <A-y> <C-O>my<C-O>*<C-O>`y
map <silent> <A-y> my*`y

" Mark current word for paste
map = viw"ay

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
noremap <F8> <ESC>:call <SID>UnderOccurences()<CR>
inoremap <F8> <C-O>:call <SID>UnderOccurences()<CR>

inoremap <A-v> <C-O>:call <SID>FindOccurences()<CR>
map <A-v> :call <SID>FindOccurences()<CR>

" Tags pattern for selection
inoremap <C-F> <C-O>:tjump <C-R><C-W><CR>
map <C-F> :tjump <C-R><C-W><CR>

" Tag back from :pop/tag
inoremap <C-B> <C-O>:pop<CR>
map <C-B> :pop<CR>

noremap <C-G> <ESC>:call <SID>ShowFunc("no")<CR>

"-----------------------------------------------------------------------
function! s:UnderOccurences()
   exe "normal [I"
   let nr = input("Which one: ")
   if nr == ""
       return
   endif
   exe "normal " . nr . "[\t"
endfunction!

function! s:FindOccurences()
   let pattern = input("Prompt Find: ")
   if pattern == ""
       return
   endif
   exe "ilist " . pattern
   let nr = input("Which one: ")
   if nr == ""
       return
   endif
   exe "ijump " . nr . pattern 
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

" from http://www.vim.org/tips/tip.php?tip_id=79 and modified
function! s:ShowFunc(sort) 
  let gf_s = &grepformat 
  let gp_s = &grepprg 
  let &grepformat='%*\k%*\sfunction%*\s%l%*\s%f %m' 
  if ( &filetype == "c" ) 
    let &grepprg = 'ctags -x --'.&filetype.'-types=f --sort='.a:sort 
  elseif ( &filetype == "vim" ) 
    let &grepprg = 'ctags -x --vim-types=f --language-force=vim --sort='.a:sort 
  elseif ( &filetype == "asm" )
    let &grepprg = 'ctags -x --asm-types=l --language-force=asm --sort='.a:sort
  endif 
  if (&readonly == 0) | update | endif 
  silent! grep % 
  cwindow 10 
  redraw   
  let &grepformat = gf_s 
  let &grepprg = gp_s 
endfunc 


" vim: shiftwidth=4
