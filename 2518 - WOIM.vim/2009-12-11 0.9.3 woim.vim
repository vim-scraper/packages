" Vim syntax and filetype plugin
" Language:		Self defined markup for WOIM lists in Vim
" Author:		Geir Isene <geir@isene.com>
" Web_site:		http://www.isene.com/
" WOIM_def:		http://www.isene.com/artweb.cgi?article=012-woim.txt 
" Version:		0.9.3
" Modified:		2009-12-08
"
" Changes since previous mod:
" Fix:  Christian Bryn <chr.bryn@gmail.com>:
"       Added 'set noexpandtab' as this is a requirement for this plugin to
"       work (i.e. needs to be set on a per file basis if not set here or in
"       vimrc).
" 
"
" INSTRUCTIONS
"
" Use only tabs/shifts for indentations
"
" Use <SPACE> to toggle one fold
" Use \0 to \9, \a, \b, \c, \d, \e, \f to show up to 15 levels expanded
"
" Use <leader><SPACE> to go to the next open template element
" (A template element is a WOIM item ending in an equal sign)
"
" Syntax updated at start and every time you leave Insert mode


" Initializing
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
set noexpandtab
set	foldmethod=syntax
set fillchars=fold:\ 
syn sync fromstart
autocmd InsertLeave * :syntax sync fromstart

" Identifier (any number in front)
syn	match	WOIMident	 "\t[0-9.]\+\.\s"				contained

" Multi-line
syn match	WOIMmulti	"\t\* "							contained

" WOIM operators
syn	match	WOIMop		"[A-ZÆØÅ _/]\{-2,}:"			contained contains=WOIMcomment

" Qualifiers are enclosed within [ ]
syn	match	WOIMqual	"\[.*\]"						contained contains=WOIMtodo,WOIMref,WOIMcomment

" Tags - anything that ends in a colon
syn	match	WOIMtag '\s\{-}[a-zA-ZæøåÆØÅ0-9,._= \-\/+<>()#"':]\{-1,}:\s' contained contains=WOIMtodo,WOIMop,WOIMcomment,WOIMref

" Mark semicolon as stringing together lines
syn match	WOIMsc		";"								contained

" References start with a hash (#)
syn	match	WOIMref	"#\{1,2}\(\'[a-zA-ZæøåÆØÅ0-9.:/ _&?%=-]\+\'\|[a-zA-ZæøåÆØÅ0-9.:/_&?%=-]\+\)" contained contains=WOIMcomment

" Comments are enclosed within ( )
syn	match	WOIMcomment	"(\_.\{-})"						contained contains=WOIMtodo,WOIMref

" Text in quotation marks
syn	match	WOIMquote	'"\_.\{-}"'						contained contains=WOIMtodo,WOIMref

" TODO  or FIXME
syn	keyword WOIMtodo	TODO FIXME						contained

" Item motion
syn match	WOIMmove	">>\|<<\|->\|<-"				contained

" Bold and Italic
syn	match   WOIMb		" \*.\{-}\* "					contained
syn	match   WOIMi		" /.\{-}/ "						contained
syn	match   WOIMu		" _.\{-}_ "						contained

" Cluster the above
syn cluster WOIMtxt contains=WOIMident,WOIMmulti,WOIMop,WOIMqual,WOIMtag,WOIMref,WOIMcomment,WOIMquote,WOIMsc,WOIMtodo,WOIMmove,WOIMb,WOIMi,WOIMu

" Levels
syn region L15 start="^\t\{14} \=\S" end="^\(^\t\{15,} \=\S\)\@!" fold contained contains=@WOIMtxt
syn region L14 start="^\t\{13} \=\S" end="^\(^\t\{14,} \=\S\)\@!" fold contained contains=@WOIMtxt,L15
syn region L13 start="^\t\{12} \=\S" end="^\(^\t\{13,} \=\S\)\@!" fold contained contains=@WOIMtxt,L14,L15
syn region L12 start="^\t\{11} \=\S" end="^\(^\t\{12,} \=\S\)\@!" fold contained contains=@WOIMtxt,L13,L14,L15
syn region L11 start="^\t\{10} \=\S" end="^\(^\t\{11,} \=\S\)\@!" fold contained contains=@WOIMtxt,L12,L13,L14,L15
syn region L10 start="^\t\{9} \=\S" end="^\(^\t\{10,} \=\S\)\@!" fold contained contains=@WOIMtxt,L11,L12,L13,L14,L15
syn region L9 start="^\t\{8} \=\S" end="^\(^\t\{9,} \=\S\)\@!" fold contained contains=@WOIMtxt,L10,L11,L12,L13,L14,L15
syn region L8 start="^\t\{7} \=\S" end="^\(^\t\{8,} \=\S\)\@!" fold contained contains=@WOIMtxt,L9,L10,L11,L12,L13,L14,L15
syn region L7 start="^\t\{6} \=\S" end="^\(^\t\{7,} \=\S\)\@!" fold contained contains=@WOIMtxt,L8,L9,L10,L11,L12,L13,L14,L15
syn region L6 start="^\t\{5} \=\S" end="^\(^\t\{6,} \=\S\)\@!" fold contained contains=@WOIMtxt,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L5 start="^\t\{4} \=\S" end="^\(^\t\{5,} \=\S\)\@!" fold contained contains=@WOIMtxt,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L4 start="^\t\{3} \=\S" end="^\(^\t\{4,} \=\S\)\@!" fold contained contains=@WOIMtxt,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L3 start="^\t\{2} \=\S" end="^\(^\t\{3,} \=\S\)\@!" fold contained contains=@WOIMtxt,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L2 start="^\t\{1} \=\S" end="^\(^\t\{2,} \=\S\)\@!" fold contained contains=@WOIMtxt,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15
syn region L1 start="^\S"       end="^\(^\t\{1,} \=\S\)\@!" fold           contains=@WOIMtxt,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15

" Folds
set foldtext=WOIMFoldText()
function! WOIMFoldText()
  let line = getline(v:foldstart)
  let myindent = indent(v:foldstart)
  let line = substitute(line, '^\s*', '', 'g')
  while myindent != 0
    let myindent = myindent - 1
    let line = ' ' . line
  endwhile
  return line
endfunction

" Highlighting and Linking :
hi				Folded			ctermfg=yellow ctermbg=none
hi				L1				gui=bold term=bold cterm=bold
hi def link		WOIMident		Define
hi def link		WOIMmulti		String
hi def link		WOIMop			Function
hi def link		WOIMqual		Type
hi def link		WOIMtag			String
hi def link		WOIMref			Define
hi def link		WOIMcomment		Comment
hi def link		WOIMquote		Comment
hi def link		WOIMsc			Type
hi def link		WOIMtodo		Todo
hi def link		WOIMmove		Todo
hi				WOIMb			ctermfg=none ctermbg=none gui=bold term=bold cterm=bold
hi				WOIMi			ctermfg=none ctermbg=none gui=italic term=italic cterm=italic
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
map <leader>a	:set foldlevel=10<CR>
map <leader>b	:set foldlevel=11<CR>
map <leader>c	:set foldlevel=12<CR>
map <leader>d	:set foldlevel=13<CR>
map <leader>e	:set foldlevel=14<CR>
map <leader>f	:set foldlevel=15<CR>
map <SPACE>		za

map <leader><SPACE>	/=\s*$<CR>A

" vim: ts=4
