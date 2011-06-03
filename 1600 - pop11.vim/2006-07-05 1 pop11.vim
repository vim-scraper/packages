" Vim syntax file
" Language:	Pop-11
" Maintainer:	Neil Madden <nem@cs.nott.ac.uk>
" Last Change:	2006 July 05
" Version:      1
" URL:          http://www.cs.nott.ac.uk/~nem/pop11.vim
"
" See http://www.poplog.org/ for information on Pop-11.

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Useful pop11 keywords
syn keyword     popKeyword    if then else endif unless endunless lvars return 
syn keyword     popKeyword    elseif until enduntil for endfor do procedure
syn keyword     popKeyword    while endwhile foreach endforeach
syn keyword     popKeyword    endprocedure lblock endlblock
syn keyword     popDecl       vars define enddefine uses 
syn keyword     popDecl       global load dlocal recordclass defclass 
syn keyword     popDecl       vectorclass constant
syn keyword     popDecl       lconstant dlvars

" Objectclass keywords (class etc should be :class)
syn keyword     popDecl       slot instance endinstance class method mixin is

" PopRuleBase keywords
syn case ignore
syn keyword     popRuleBase   RULE NOT LVARS REPLACE WHERE DEL POP11 DLOCAL 
syn keyword     popRuleBase   VARS DO CUT OR INDATA NOT_EXISTS IMPLIES ALL
syn keyword     popRuleBase   FILTER ADD TESTADD ADDALL ADDIF ADDUNLESS SAY
syn keyword     popRuleBase   SAYIF EXPLAIN STOP STOPIF QUIT QUITIF PUSH POP
syn keyword     popRuleBase   REPLACE MODIFY RMODIFY NULL TYPE SAVE RESTORE
syn keyword     popRuleBase   PUSHRULES POPRULES PUSHDATA POPDATA SELECT MAP
syn keyword     popRuleBase   DOALL FAIL PAUSE READ MENU NEWLIMIT
syn case match

syn keyword     popTodo       contained TODO FIXME XXX

" String constants
syn region      popString       start=+L\="+ skip=+\\\\\|\\"+ end=+"+
syn region      popString2      start=+L\='+ skip=+\\\\\|\\'+ end=+'+
" Not sure if these number REs are correct
syn case ignore
syn match       popNumber       "\<[+-]\=\d\+\(u\=l\=\|lu\|f\)\>"
syn match	popNumber	"\<[+-]\=\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\=\>"
syn match	popNumber	"\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
syn match	popNumber	"\<[+-]\=\d\+e[-+]\=\d\+[fl]\=\>"
syn case match

syn region      popComment	display start="/\*" end="\*/" contains=popTodo
syn region	popCommentL	start=";;;" skip="\\$" end="$" keepend contains=popTodo
syn keyword	popOperator	mod and or not from to in
syn keyword     popConstant     true false

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_pop_syn_inits")
  if version < 508
    let did_pop_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink popDecl                Structure
  HiLink popKeyword             Keyword
  HiLink popOperator            Operator
  HiLink popRuleBase            Macro
  HiLink popTodo                Todo
  HiLink popString              String
  HiLink popString2             String
  HiLink popNumber              Number
  HiLink popComment             Comment
  HiLink popCommentL            Comment
  HiLink popConstant            Constant

  delcommand HiLink
endif

let b:current_syntax = "pop11"
