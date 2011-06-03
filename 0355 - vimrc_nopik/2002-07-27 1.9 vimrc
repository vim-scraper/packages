" Configuration file for gvim
" Written for Debian GNU/Linux by W.Akkerman <wakkerma@debian.org>
" Some modifications by J.H.M. Dassen <jdassen@wi.LeidenUniv.nl>
" Heavily extended by Kamil "Nopik" Burzynski <nopik@free.of.pl> 

" Normally we use vim-extensions. If you want true vi-compatibility
" remove change the following statements
set nocompatible	" Use Vim defaults (much better!)
set backspace=2		" allow backspacing over everything in insert mode
" Now we set some defaults for the editor 
" set autoindent		" always set autoindenting on
set textwidth=0		" Don't wrap words by default
"set nobackup		" Don't keep a backup file
set viminfo='20,\"50	" read/write a .viminfo file, don't store more than
			" 50 lines of registers
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time

" Suffixes that get lower priority when doing tab completion for filenames.
" These are files we are not likely to want to edit or read.
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
" We know xterm-debian is a color terminal
if &term =~ "xterm-debian" || &term =~ "xterm-xfree86" || &term =~ "xterm"
  set t_Co=16
  set t_Sf=[3%dm
  set t_Sb=[4%dm
endif

" Vim5 comes with syntaxhighlighting. If you want to enable syntaxhightlighting
" by default uncomment the next three lines. 
if has("syntax")
  syntax on		" Default to no syntax highlightning 
endif

inoremap  
cnoremap  
imap [4~ <End>
cmap [4~ <End>

" Debian uses compressed helpfiles. We must inform vim that the main
" helpfiles is compressed. Other helpfiles are stated in the tags-file.
set helpfile=$VIMRUNTIME/doc/help.txt.gz

"Autogroups{{{"
if has("autocmd")

" Set some sensible defaults for editing C-files
augroup cprog
  " Remove all cprog autocommands
  au!

  " When starting to edit a file:
  "   For *.c and *.h files set formatting of comments and set C-indenting on.
  "   For other files switch it off.
  "   Don't change the order, it's important that the line with * comes first.
  autocmd BufRead *       set tabstop=2 shiftwidth=2 formatoptions=tcql nocindent comments&
  autocmd BufRead *.c,*.h,*.cpp,*.cxx set formatoptions=croql cindent comments=sr:/*,mb:*,el:*/,://

iab #i #include
iab #f #if
iab #d #define
iab #e #endif
iab #l #else
iab ret return( );<C-o>2<Left>
iab el else<CR>{

set sc

augroup END
   " curlyC.vim
   " From Long Truong : Vim Tip #125.
   " Automatically comments {} in C/C++/Java.
   au BufNewFile,BufRead *.c,*.cc,*.h imap }<CR> <ESC>:call CurlyBracket()<CR>a
   au BufNewFile,BufRead *.cpp,*.C imap }<CR> <ESC>:call CurlyBracket()<CR>a
   au BufNewFile,BufRead *.java,*.idl imap }<CR> <ESC>:call CurlyBracket()<CR>a
   function CurlyBracket()
       let l:startline = line(".")
       let l:result1 =  searchpair('{', '', '}', 'bW')
       if (result1 > 0)
           let l:linenum = line(".")
           let l:string1 = substitute(getline(l:linenum), '^\s*\(.*\)\s*$', '\1', "")
           if (l:string1 =~ '^{')
               let l:string1 = substitute(getline(l:linenum - 1), '^\s*\(.*\)\s*$', '\1', "") . " " . l:string1
               sil exe "normal k"
           endif
           " get else part if necessary
           if (l:string1 =~ "else" )
               sil exe "normal k0"
               let l:result2 =  searchpair('{', '', '}', 'bW')
               if (l:result2 > 0)
                   let l:linenum = line(".")
                   let l:string2 = substitute(getline(l:linenum - 1), '^\s*\(.*\)\s*$', '\1', "")
                   if (l:string2 =~ '^{')
                       let l:string2 = substitute(getline(l:linenum - 1), '^\s*\(.*\)\s*$', '\1', "") . " " . l:string2
                   endif
                   let l:string1 = l:string2 . " ... " . l:string1
               endif
           endif
           " remove trailing whitespaces and curly brace
           let l:my_string = substitute(l:string1, '\s*{[^{]*$', '', "")
           let l:my_strlen = strlen(l:my_string)
           if (l:my_strlen > 30)
               let l:my_string = strpart(l:my_string,0,30)."..."
           endif
           sil exe ":" . l:startline
           sil exe "normal i}"
           "if ((l:startline - l:linenum) > 10)
               sil exe "normal a /* " . l:my_string . " */"
           "endif
       endif
   endfunction

" Also, support editing of gzip-compressed files. DO NOT REMOVE THIS!
" This is also used when loading the compressed helpfiles.
augroup gzip
  " Remove all gzip autocommands
  au!

  " Enable editing of gzipped files
  "	  read:	set binary mode before reading the file
  "		uncompress text in buffer after reading
  "	 write:	compress file after writing
  "	append:	uncompress file, append, compress file
  autocmd BufReadPre,FileReadPre	*.gz set bin
  autocmd BufReadPre,FileReadPre	*.gz let ch_save = &ch|set ch=2
  autocmd BufReadPost,FileReadPost	*.gz '[,']!gunzip
  autocmd BufReadPost,FileReadPost	*.gz set nobin
  autocmd BufReadPost,FileReadPost	*.gz let &ch = ch_save|unlet ch_save
  autocmd BufReadPost,FileReadPost	*.gz execute ":doautocmd BufReadPost " . expand("%:r")

  autocmd BufWritePost,FileWritePost	*.gz !mv <afile> <afile>:r
  autocmd BufWritePost,FileWritePost	*.gz !gzip <afile>:r

  autocmd FileAppendPre			*.gz !gunzip <afile>
  autocmd FileAppendPre			*.gz !mv <afile>:r <afile>
  autocmd FileAppendPost		*.gz !mv <afile> <afile>:r
  autocmd FileAppendPost		*.gz !gzip <afile>:r
augroup END

augroup bzip2
  " Remove all bzip2 autocommands
  au!

  " Enable editing of bzipped files
  "       read: set binary mode before reading the file
  "             uncompress text in buffer after reading
  "      write: compress file after writing
  "     append: uncompress file, append, compress file
  autocmd BufReadPre,FileReadPre        *.bz2 set bin
  autocmd BufReadPre,FileReadPre        *.bz2 let ch_save = &ch|set ch=2
  autocmd BufReadPost,FileReadPost      *.bz2 |'[,']!bunzip2
  autocmd BufReadPost,FileReadPost      *.bz2 let &ch = ch_save|unlet ch_save
  autocmd BufReadPost,FileReadPost      *.bz2 execute ":doautocmd BufReadPost " . expand("%:r")

  autocmd BufWritePost,FileWritePost    *.bz2 !mv <afile> <afile>:r
  autocmd BufWritePost,FileWritePost    *.bz2 !bzip2 <afile>:r

  autocmd FileAppendPre                 *.bz2 !bunzip2 <afile>
  autocmd FileAppendPre                 *.bz2 !mv <afile>:r <afile>
  autocmd FileAppendPost                *.bz2 !mv <afile> <afile>:r
  autocmd FileAppendPost                *.bz2 !bzip2 -9 --repetitive-best <afile>:r
augroup END
" vim -b : edit binary using xxd-format!
augroup Binary
  au!
  au BufReadPre  *.bin let &bin=1
  au BufReadPost *.bin if &bin | %!xxd
  au BufReadPost *.bin set ft=xxd | endif
  au BufWritePre *.bin if &bin | %!xxd -r
  au BufWritePre *.bin endif
  au BufWritePost *.bin if &bin | %!xxd
  au BufWritePost *.bin set nomod | endif
augroup END

endif " has ("autocmd")

" Some Debian-specific things
augroup filetype
  au BufRead reportbug.*		set ft=mail
augroup END
"}}}

" The following are commented out as they cause vim to behave a lot
" different from regular vi. They are highly recommended though.
set showcmd		" Show (partial) command in status line.
"set showmatch		" Show matching brackets.
set ignorecase
set smartcase		" Do case insensitive matching
set incsearch		" Incremental search
"set autowrite		" Automatically save before commands like :next and :make
set nohls
set hh=20
set statusline=%<%f%h%m%r\ %y\ %w(%b,0x%B)%=%l(of\ %L),%c%V\ %P
set laststatus=2
let &foldmethod="marker"
colorscheme ron
highlight Folded ctermfg=8 ctermbg=0
set number
highlight LineNr ctermfg=6
set sessionoptions=blank,buffers,curdir,folds,help,localoptions,winsize
set path=,/usr/include,/usr/src/dev/gcc2/include/g++-3
set previewheight=15
let g:explDetailedList=1

nmap <Insert> za
imap <Insert> za

set tags+=~/.vimsessions/tags

imap <F2> <Esc><F2>
nmap <F2> :call ScrollPrev()<CR>
imap <S-F2> <Esc><S-F2>
nmap <S-F2> :bprev<CR>
imap <F3> <Esc><F3>
nmap <F3> :call ScrollNext()<CR>
imap <S-F3> <Esc><S-F3>
nmap <S-F3> :bnext<CR>
imap <F4> <Esc><F4>
nmap <F4> :call DelArg()<CR>
imap <S-F4> <Esc><S-F4>
nmap <S-F4> :bdel<CR>:bnext<CR>
imap <F5> <Esc><F5>
nmap <F5> :buffers<CR>
nmap <S-F5> :buffers<CR>:exe ":buf " . input("Which one: ")<CR>
imap <S-F5> <Esc><S-F5>
imap <F6> <Esc><F6>
nmap <F6> :call AddArg()<CR>
imap <S-F6> <Esc><S-F6>
"nmap <S-F6> :exe "norma :bad ".input("Enter filename to add: ")."\n:blast\n"<CR>
nmap <S-F6> :Sexplore<CR><C-w>o
nmap <F7> <Down>^]}i<End><CR>//}}}<Esc><Up>^[{<Up>ddPP$i<Right>{{{<Esc>^i//<Esc>za<Ins><Down>
imap <F7> <Esc><F7>i
nmap <S-F7> <Up>dd<Down>^]}<Down>dd<Up>[{<Up><F7>
imap <S-F7> <Esc><S-F7>i
nmap <F8> :so ~/.vimsessions/
nmap <S-F8> :wa<Bar>exe "mksession! " . input( "Session name: ", "~/.vimsessions/".fnamemodify(v:this_session, ":p:t"))<CR>
imap <F9> <Esc><F9>
nmap <F9> :w<CR>:chdir %:p:h<CR>:make<CR>
imap <S-F9> <Esc><S-F9>
nmap <S-F9> :chdir %:p:h<CR>:!make clean<CR>
imap <F10> <Esc><F10>
nmap <F10> :w<CR>:chdir %:p:h<CR>:!make<CR>
imap <S-F10> <Esc><S-F10>
nmap <S-F10> :up<CR>:chdir %:p:h<CR>:!make run<CR>
nmap <F11> :w<CR><Bar>:!ctags -a --c-types=cdefgmnstuvx -f ~/.vimsessions/tags %<CR><CR>
imap <F11> <Esc><F11>


if &term =~ "xterm"
	imap [3;5~ <Esc>[39~i
	nmap [3;5~ [39~
	imap Ü <Esc><Bslash>i<Right>
	nmap Ü <Bslash>
	nmap	<S-F6> [32~
	imap	<S-F6> [32~
endif

imap [39~ <Esc>[39~i
nmap [39~ dd

nmap <C-f> [I:exe "normal " . input("Which one: ") ."[\t"<CR>
nmap f [i

imap <C-w> <Esc>:w<CR>i<Right>

nmap <S-a> <C-a>
nmap <S-x> <C-x>

imap \ <Esc><Bslash>i<Right>
nmap \ :call SwapComment()<CR>
vmap \ :call SwapComment()<CR>

"Ovewriting existing commands.
"imap <C-e> <C-e>
"nmap <C-e> gf

nmap <C-w>d	:bdel<CR>

"Comments
vmap C c/**/<Left><Left><C-o>P<Esc>
vmap <C-c> x2<Left>P<Right>4x

"{{{CVS integration"
nmap <C-c>d :chdir %:p:h<CR>:!cvs diff -w %:t<Bar>patch -R -o %:t.cvs<CR>:vert diffsplit %:t.cvs<CR><CR><C-W><Right>:echo "CVS diff mode turned on."<CR>
nmap <C-c>D	:set nodiff<CR>:set foldcolumn=0<CR>:set foldmethod=marker<CR><C-W><Left>:bwip<CR>:echo "CVS diff mode turned off."<CR>

nmap <F12> :w<CR>:let cvsfile = @%<CR>:bd<CR>:let @z = ":!cvs commit " . cvsfile<CR>@z<CR>:let @z = ":badd " . cvsfile<CR>:let @z = ":buffer " . cvsfile<CR>@z<CR> 
imap <F12> <Esc><F12>
"}}}

"These codes are for alt+shift+cursors.
"Warning: these are nonstandard codes, I modified keymap manually.
"These sequences are defining very nice line movement.
imap <M-S-Left> <C-d>
nmap <M-S-Left> i<Esc>
imap <M-S-Right> <C-t>
nmap <M-S-Right> i<Esc>
imap <M-S-Down> <Esc>[a~i<Right>
nmap <M-S-Down> ddp
imap <M-S-Up> <Esc>[9~i<Right>
nmap <M-S-Up> <Up>ddp<Up>
nmap = 1G=G``
imap = 1G=G``

set gfn=-b&h-lucidatypewriter-medium-r-normal-*-*-120-*-*-m-*-iso8859-1

source $VIMRUNTIME/menu.vim
set wildmenu
set cpo-=<
set wcm=<C-Z>
"map <F9> :emenu <C-Z>

"Nopik's functions ;){{{"
fu SwapComment()
 let c = col(".")-1
 exe "normal ^"
 let s = getline(".")[col(".")-1] . getline(".")[col(".")]
 if s == "//"
	exe "normal 2x"
	let c = c - 2
 else
	exe "normal! i//"
	let c = c + 2
 endif
 let d = col(".")-1
 if d > 0
  if c > 0
 	exe "normal " . d . "h" . c . "l"
  else
   exe "normal " . d . "h"
  endif
 else
  if c > 0
   exe "normal " . c . "l"
  endif
 endif
endf

fu ScrollNext()
   let i = 0
	let f = 0
   while i<argc()
      if argv(i) == @%
			let f = 1
      endif
      let i = i + 1
   endwhile

	if f == 0
		exe "last "
	else
 		if argc() > 1
			if @% == argv(argc()-1)
				exe "rewind "
			else
				exe "next "
			endif
		endif
 	endif
endf

fu ScrollPrev()
   let i = 0
	let f = 0
   while i<argc()
      if argv(i) == @%
         let f = 1
      endif
      let i = i + 1
   endwhile

	if f == 0
		exe "rewind "
	else
		if argc() > 1
			if @% == argv(0)
				exe "last "
			else
				exe "previous "
			endif
		endif
	endif
endf

fu AddArg()
  let i = 0
  let f = 0
  let s = ""
  while i<argc()
	 if argv(i) == @%
		let f = 1
	 endif   
    let s = s . " " . argv(i)
    let i = i + 1
  endwhile
  if f == 0
  	 let s = s . " " . @%
  	 exe "args " . s
  	 exe "last "
  endif
endf 

fu DelArg()
	let i = 0
	let s = ""
	while i<argc()
		if argv(i) != @%
			let s = s . " " . argv(i)
		endif
		let i = i + 1
	endwhile
	exe "args " . s
endf
"}}}

