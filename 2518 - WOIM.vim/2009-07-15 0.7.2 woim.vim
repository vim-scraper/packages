" Vim syntax and macro file
" Language :	Self defined markup for WOIM lists in Vim
" Author:		Geir Isene <geir@isene.com>
" Last mod.:	2009-07-15
"
" Changes since last mod:
"	Changed references (WOIMref) to highlight as "Define"
"	Added syntax highlighting for vim set-lines
"
" Use only tabs or shifts for indentations
" Use \0 to \9 to show the list with that many levels expanded
" Use <SPACE> to toggle one fold
" Syntax updates every time you leave Insert mode
" Use <leader><SPACE> to go to the next open template element
" A template element is a WOIM item ending in an equal sign

if exists("b:current_syntax")
  finish
endif

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Basics
let	b:current_syntax="WOIM"
set	shiftwidth=2
set	tabstop=2
set	softtabstop=2
set	foldmethod=syntax
set fillchars=fold:\ 
autocmd InsertLeave * :syntax sync fromstart

" Attributes - anything that ends in a colon
syn	match	WOIMattr "\s\{-}[a-zA-ZæøåÆØÅ0-9,_= \-]\+:\s"	contained contains=WOIMtodo,WOIMkey

" Index (any number in front)
syn	match	WOIMindex "\t[0-9.]\+\s"					contained

" Conditions are enclosed within [ ]
syn	match	WOIMcond	"\[.*\]"						contained contains=WOIMtodo

" Comments are enclosed within ( )
syn	match	WOIMcomment	"(.\{-})"						contained contains=WOIMtodo,WOIMref

" References start with a hash (#)
syn	match	WOIMref		"#[a-zA-ZæøåÆØÅ0-9.:/ _]\+"		contained contains=WOIMcomment

" WOIM operators
syn	match	WOIMkey		"[A-ZÆØÅ _]\+:"					contained

" Mark semicolon as stringing together lines
syn match	WOIMsc		";"								contained

" Multiline
syn match	WOIMmulti	"\s\* "							contained

" TODO  or FIXME
syn	keyword WOIMtodo TODO FIXME							contained

" Bold and Italic
syn	match   WOIMb		" \*.\{-}\* "					contained
syn	match   WOIMi		" /.\{-}/ "						contained
syn	match   WOIMu		" _.\{-}_ "						contained

" Cluster the above
syn cluster WOIMtxt contains=WOIMindex,WOIMattr,WOIMcond,WOIMcomment,WOIMref,WOIMmulti,WOIMkey,WOIMsc,WOIMtodo,WOIMb,WOIMi,WOIMu

" Levels
syn region L9 start="^\t\{8}\S" end="^\(^\t\{9,}\S\)\@!" fold contained contains=@WOIMtxt
syn region L8 start="^\t\{7}\S" end="^\(^\t\{8,}\S\)\@!" fold contained contains=@WOIMtxt,L9
syn region L7 start="^\t\{6}\S" end="^\(^\t\{7,}\S\)\@!" fold contained contains=@WOIMtxt,L8,L9
syn region L6 start="^\t\{5}\S" end="^\(^\t\{6,}\S\)\@!" fold contained contains=@WOIMtxt,L7,L8,L9
syn region L5 start="^\t\{4}\S" end="^\(^\t\{5,}\S\)\@!" fold contained contains=@WOIMtxt,L6,L7,L8,L9
syn region L4 start="^\t\{3}\S" end="^\(^\t\{4,}\S\)\@!" fold contained contains=@WOIMtxt,L5,L6,L7,L8,L9
syn region L3 start="^\t\{2}\S" end="^\(^\t\{3,}\S\)\@!" fold contained contains=@WOIMtxt,L4,L5,L6,L7,L8,L9
syn region L2 start="^\t\{1}\S" end="^\(^\t\{2,}\S\)\@!" fold contained contains=@WOIMtxt,L3,L4,L5,L6,L7,L8,L9
syn region L1 start="^\S"       end="^\(^\t\{1,}\S\)\@!" fold           contains=@WOIMtxt,L2,L3,L4,L5,L6,L7,L8,L9

" Folds
set foldtext=WOIMFoldText()
function! WOIMFoldText()
  let line = getline(v:foldstart)
  let myindent = indent(v:foldstart)
  let line = substitute(line, '^\s*', '', 'g')
"  let line = '+ ' . line
  while myindent != 0
    let myindent = myindent - 1
    let line = ' ' . line
  endwhile
  return line
endfunction

" Highlighting and Linking :
hi				Folded			ctermfg=yellow ctermbg=none
hi				L1				gui=bold term=bold cterm=bold
hi def link		WOIMattr		String
hi def link		WOIMindex		Statement
hi def link		WOIMcond		Type
hi def link		WOIMkey			Function
hi def link		WOIMsc			Type
hi def link		WOIMtodo		Todo
hi def link		WOIMcomment		Comment
hi def link		WOIMref			Define
hi def link		WOIMmulti		String
hi				WOIMi			ctermfg=none ctermbg=none gui=italic term=italic cterm=italic
hi				WOIMb			ctermfg=none ctermbg=none gui=bold term=bold cterm=bold
hi				WOIMu			ctermfg=none ctermbg=none gui=underline term=underline cterm=underline

" VIM parameters
syn	match		WOIMvim			"^vim:.*"
hi def link		WOIMvim			Function

" macros
map <leader>0	:set foldlevel=0<CR>
map <leader>1	:set foldlevel=1<CR>
map <leader>2	:set foldlevel=2<CR>
map <leader>3	:set foldlevel=3<CR>
map <leader>4	:set foldlevel=4<CR>
map <leader>5	:set foldlevel=5<CR>
map <leader>6	:set foldlevel=6<CR>
map <leader>7	:set foldlevel=7<CR>
map <leader>8	:set foldlevel=8<CR>
map <leader>9	:set foldlevel=9<CR>
map <SPACE>		za

map <leader><SPACE>	/=\s*$<CR>A

" vim: ts=4
