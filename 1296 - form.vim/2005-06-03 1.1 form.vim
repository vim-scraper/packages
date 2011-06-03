" Vim syntax file
" Language:	FORM
" Version: 1.1
" Maintainer:	Michael M. Tung <michael.tung@uni-mainz.de>
" Last Change: <Fri Jun 03 13:59:28 CEST 2005>
" Past Change: <Wed, 2005/05/25 09:24:58 arwagner wptx44>

" First public release based on 'Symbolic Manipulation with FORM'
" by J.A.M. Vermaseren, CAN, Netherlands, 1991.
" This syntax file is still in development. Please send suggestions
" to the maintainer.

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" A bunch of useful FORM keywords
syn keyword formType		global local
syn keyword formHeaderStatement	symbol symbols cfunction cfunctions
syn keyword formHeaderStatement	function functions vector vectors
syn keyword formHeaderStatement	set sets index indices
syn keyword formHeaderStatement	dimension dimensions unittrace
syn keyword formStatement	id identify drop skip
syn keyword formStatement	write nwrite
syn keyword formStatement	format print nprint load save
syn keyword formStatement	bracket brackets
syn keyword formStatement	multiply count match only discard
syn keyword formStatement	trace4 traceN contract symmetrize antisymmetrize
syn keyword formConditional	if else elseif endif while
syn keyword formConditional	repeat endrepeat label goto
syn keyword formConditional     argument endargument

" some special functions
syn keyword formStatement	g_ gi_ g5_ g6_ g7_ 5_ 6_ 7_
syn keyword formStatement	e_ d_ delta_ theta_ sum_ sump_

" pattern matching for keywords
syn match   formComment		"^\ *\*.*$"
syn match   formComment		"\;\ *\*.*$"
syn region  formString		start=+"+  end=+"+
syn region  formString		start=+'+  end=+'+
syn region  formString		start=+`+  end=+'+
syn match   formPreProc		"^\=\#[a-zA-z][a-zA-Z0-9]*\>"
syn match   formNumber		"\<\d\+\>"
syn match   formNumber		"\<\d\+\.\d*\>"
syn match   formNumber		"\.\d\+\>"
syn match   formNumber		"-\d" contains=Number
syn match   formNumber		"-\.\d" contains=Number
syn match   formNumber		"i_\+\>"
syn match   formNumber		"fac_\+\>"
" pattern matching wildcards
syn match   formNumber		"?[A-z0-9]*"
" dollar-variables (new in 3.x)
syn match   formNumber		"$[A-z0-9]*"
" scalar products
syn match   formNumber		"^\=[a-zA-z][a-zA-Z0-9]*\.[a-zA-z][a-zA-Z0-9]*\>"

syn match   formDirective	"^\=\.[a-zA-z][a-zA-Z0-9]*\>"

" hi User Labels
syn sync ccomment formComment minlines=10

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_form_syn_inits")
  if version < 508
    let did_form_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink formConditional	Conditional
  HiLink formNumber		Number
  HiLink formStatement		Statement
  HiLink formComment		Comment
  HiLink formPreProc		PreProc
  HiLink formDirective		PreProc
  HiLink formType		Type
  HiLink formString		String

  if !exists("form_enhanced_color")
    HiLink formHeaderStatement	Statement
  else
  " enhanced color mode
    HiLink formHeaderStatement	HeaderStatement
    " dark and a light background for local types
    if &background == "dark"
      hi HeaderStatement term=underline ctermfg=LightGreen guifg=LightGreen gui=bold
    else
      hi HeaderStatement term=underline ctermfg=DarkGreen guifg=SeaGreen gui=bold
    endif
    " change slightly the default for dark gvim
    if has("gui_running") && &background == "dark"
      hi Conditional guifg=LightBlue gui=bold
      hi Statement guifg=LightYellow
    endif
  endif

  delcommand HiLink
endif

  let b:current_syntax = "form"

" vim: ts=8
