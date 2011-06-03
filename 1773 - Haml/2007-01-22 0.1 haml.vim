" Vim syntax file
" Language:		Haml (XHTML Abstraction Markup Language) 
" Maintainer:		Dmitry A. Ilyashevich <dmitry.ilyashevich@gmail.com>
" Version:              0.1

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'haml'
endif

if version < 600
  so <sfile>:p:h/html.vim
  syn include @rubyTop <sfile>:p:h/ruby.vim
else
  runtime! syntax/html.vim
  unlet b:current_syntax
  syn include @rubyTop syntax/ruby.vim
endif

" TODO: Multiline ruby code
syn region  hamlRubyCode    matchgroup=Delimiter  start="[=~]" end="$" contains=@rubyTop keepend
syn region  hamlRubyCode    matchgroup=Delimiter  start="- " end="$" contains=@rubyTop keepend
syn region  hamlRubyHash    matchgroup=Delimiter  start="{" end="}" contains=@rubyTop keepend
syn region  hamlRubyHash    matchgroup=Delimiter  start="\[" end="\]" contains=@rubyTop keepend

syn region  hamlHtmlTag     start="%[a-zA-Z0-9]*[=~]" end="$" contains=@rubyTop,hamlRubyCode,hamlRubyHash
syn match   hamlHtmlTag     "%[a-zA-Z0-9]*"
syn match   hamlHtmlTag     "/$"

syn region  hamlCssClass    start="\.[a-zA-Z0-9]*[=~]" end="$" contains=@rubyTop,hamlRubyCode,hamlRubyHash
syn match   hamlCssClass    "\.[a-zA-Z0-9]*"

syn region  hamlCssId       start="#[a-zA-Z0-9]*[=~]" end="$" contains=@rubyTop,hamlRubyCode,hamlRubyHash
syn match   hamlCssId       "#[a-zA-Z0-9]*"

" TODO: Multiline ' ~' text
" TODO: Multiline comments
syn region  hamlPrePred     matchgroup=Delimiter  start="\~" end="$" oneline
syn region  hamlPrePred     matchgroup=Delimiter  start="^" end="|$" oneline
syn match   hamlPrePred     "\\."
syn match   hamlComment     "^!!!.*$"
syn match   hamlComment	    " /.*$"               contains=hamlTodo,@Spell
syn match   hamlComment	    "^/.*$"               contains=hamlTodo,@Spell
syn keyword hamlTodo        TODO FIXME XXX        contained


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_haml_syntax_inits")
  if version < 508
    let did_haml_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink hamlPreProc            PreProc
  HiLink hamlHtmlTag            Statement
  HiLink hamlCssClass           Type
  HiLink hamlCssId              Type
  HiLink hamlComment		Comment
  HiLink hamlTodo               Todo

  delcommand HiLink
endif
let b:current_syntax = "haml"

if main_syntax == 'haml'
  unlet main_syntax
endif

" vim: nowrap sw=2 sts=2 ts=8 ff=unix:
