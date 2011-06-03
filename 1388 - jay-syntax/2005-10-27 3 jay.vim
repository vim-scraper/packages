" Vim syntax file
" Language:	Jay
" Maintainer:	Jelmer Vernooij <jelmer@samba.org>
" Last Change:	27 Oct, 2005
" Version:	3
"
" Based on yacc.vim by Dr. Charles E. Campbell, Jr. 
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Read the C syntax to start with
if version >= 600
    runtime! syntax/cs.vim
else
  so <sfile>:p:h/cs.vim
endif

" Clusters
syn cluster	jayActionGroup	contains=jayDelim,cInParen,cTodo,cIncluded,jayDelim,jayCurlyError,jayUnionCurly,jayUnion,cUserLabel,cOctalZero,cCppOut2,cCppSkip,cErrInBracket,cErrInParen,cOctalError,cCommentStartError,cParenError
syn cluster	jayUnionGroup	contains=jayKey,cComment,jayCurly,cType,cStructure,cStorageClass,jayUnionCurly

" jay stuff
syn match	jayDelim	"^\s*[:|;]"
syn match	jayOper	"@\d\+"

syn match	jayKey	"^\s*%\(token\|type\|left\|right\|start\|ident\|nonassoc\)\>"
syn match	jayKey	"\s%\(prec\|expect\)\>"
syn match	jayKey	"\$\(<[a-zA-Z_][a-zA-Z_0-9]*>\)\=[\$0-9]\+"
syn keyword	jayKeyActn	yyerrok yyclearin

syn match	jayUnionStart	"^%union"	skipwhite skipnl nextgroup=jayUnion
syn region	jayUnion	contained matchgroup=jayCurly start="{" matchgroup=jayCurly end="}"	contains=@jayUnionGroup
syn region	jayUnionCurly	contained matchgroup=jayCurly start="{" matchgroup=jayCurly end="}" contains=@jayUnionGroup
syn match	jayBrkt	contained "[<>]"
syn match	jayType	"<[a-zA-Z_][a-zA-Z0-9_]*>"	contains=jayBrkt
syn match	jayDefinition	"^[A-Za-z][A-Za-z0-9_]*[ \t]*:"

" special jay separators
syn match	jaySectionSep	"^[ \t]*%%"
syn match	jaySep	"^[ \t]*%{"
syn match	jaySep	"^[ \t]*%}"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_jay_syn_inits")
  if version < 508
    let did_jayhdl_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " Internal jay highlighting links
  HiLink jayBrkt	jayStmt
  HiLink jayKey	jayStmt
  HiLink jayOper	jayStmt
  HiLink jayUnionStart	jayKey

  " External jay highlighting links
  HiLink jayCurly	Delimiter
  HiLink jayCurlyError	Error
  HiLink jayDefinition	Function
  HiLink jayDelim	Function
  HiLink jayKeyActn	Special
  HiLink jaySectionSep	Todo
  HiLink jaySep	Delimiter
  HiLink jayStmt	Statement
  HiLink jayType	Type

  " since Bram doesn't like my Delimiter :|
  HiLink Delimiter	Type

  delcommand HiLink
endif

let b:current_syntax = "jay"

" vim: ts=15
