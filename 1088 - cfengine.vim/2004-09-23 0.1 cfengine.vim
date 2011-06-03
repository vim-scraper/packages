" Vim syntax file
" Language:	Cfengine configure file
" Maintainer:	Christain Pearce <christian@pearcec.com>
" Last Change:	2004 Sep 22

" 1. Add this file to ~/.vim/sytnax/
"
" 2. Add the following to ~/.vim/scripts.vim
"
"     if did_filetype()
"      finish
"    endif
"    if search('^#\s*vim:\s*set\s*syntax=cfengine')
"      setf cfengine
"    endif
"
" 3. Put the following anywhere in the comments of a cfengine file
" to automatically load cfengine highlighting.
"
"   vim: set syntax=cfengine
"


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Include PHP syntax hightlighting
if version < 600
  syn include @phpTop <sfile>:p:h/php.vim
else
  syn include @phpTop syntax/php.vim
endif
syn sync clear
unlet b:current_syntax

syn match       cfengineClassOperator        "[$!&|.()]"       contained display
syn match       cfengineVarSelector     "[$(){}]"       contained display

syn keyword	cfegineTodo	contained TODO FIXME XXX
" Avoid matching "text#text", used in /etc/disktab and /etc/gettytab
syn match	cfengineComment	"^#.*" contains=cfengineTodo
syn match	cfengineComment	"\s#.*"ms=s+1 contains=cfengineTodo
syn region      cfengineIdentifier  start="\${" end="}" oneline contains=cfengineVarSelector keepend extend
syn region      cfengineIdentifier  start="\$(" end=")" oneline contains=cfengineVarSelector keepend extend
"syn match      cfengineIdentifier  "${\h\w*}" contains=cfengineVarSelector contained extend
syn region      cfenginePHP     start="<?" end="?>" contains=@phpTop keepend extend
syn match	cfengineSection	"\s*\a\+:\s*$"he=e-1
syn match       cfengineClass   "\s*\S*::\s*$"he=e-2 contains=cfengineClassOperator
syn region	cfengineString	start=+"+ skip=+\\\\\|\\"+ end=+"+ oneline contains=cfengineIdentifier keepend extend
syn region	cfengineString	start=+'+ skip=+\\\\\|\\'+ end=+'+ oneline contains=cfengineIdentifier keepend extend

" Do something for equals


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_conf_syntax_inits")
  if version < 508
    let did_conf_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink cfengineComment	Comment
  HiLink cfengineTodo	Todo
  HiLink cfengineString	String
  HiLink cfengineIdentifier	Identifier
  HiLink cfengineSection    Statement
  HiLink cfengineClassOperator    Operator
  HiLink cfengineVarSelector    Operator
  HiLink cfengineClass  Special

  delcommand HiLink
endif

let b:current_syntax = "cfengine"

" vim: ts=8 sw=2
