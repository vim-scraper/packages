" Vim syntax file
" Languages:	Eyapp and Yapp
" Maintainer:	Casiano Rodriguez-Leon <casiano.rodriguez.leon@gmail.com>
" Last Change:	nov 15 WET 2008
" Version:	1
" URL: http://nereida.deioc.ull.es/~vim/
" License: Same as Vim
" Limitations: If the .yp/.eyp file is really big you may need to increase 'maxmempattern'
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Set variable eyapp_only if you only want to emphasize grammar vs actions
if !exists("g:eyapp_only")
  syntax include @perl syntax/perl.vim
endif

" Eyapp stuff
syn match	eyappDelim	"^\s*[:|;]"
syn match	eyappOper	"@\d\+"

syn match	eyappKey	"^\s*%\(token\|semantic|syntactic|type\|left\|right\|start\|ident\|nonassoc\)\>"
syn match	eyappKey	"\s%\(prec\|expect|name|tree|metatree|begin\)\>"
syn match	eyappKey	"\$\(<[a-zA-Z_][a-zA-Z_0-9]*>\)\=[\$0-9]\+"

syn keyword	eyappKeyMethod	YYErrok yyclearin YYAbort error YYParse YYData YYNberr YYRecovering YYAbort YYAccept YYError YYSemval YYLhs       contained
syn keyword     eyappKeyMethod    YYRuleindex YYRightside YYIsterm YYIssemantic YYName YYPrefix YYAccessors YYFilename YYBypass YYBypassrule Y    contained
syn keyword     eyappKeyMethod    YFirstline YYBuildingTree YYBuildAST YYBuildTS YYCurtok YYCurval YYExpect YYLexer                               contained

syn match       eyappKey        "%defaultaction" nextgroup=eyappAction
syn match	eyappUnionStart	"^%union"	skipwhite skipnl nextgroup=eyappAction

syn match	eyappBrkt	contained "[<>]"
syn match	eyappType	"<[a-zA-Z_][a-zA-Z0-9_]*>"	contains=eyappBrkt
syn match	eyappDefinition	"^[A-Za-z][A-Za-z0-9_]*\_s*:"

syn match       eyappModifier   "[+*?()<>%]" 
syn match       eyappAttribute  "\.\s*[a-zA-Z_][a-zA-Z0-9_]*"
syn match       eyappId         "[a-zA-Z_][a-zA-Z0-9_]*"
syn match       eyappString     "'\([^\\']\|\\.\)*'"

" special Eyapp separators
syn match	eyappSectionSep	"^[ \t]*%%"
syn match	eyappSep	"^[ \t]*%{"
syn match	eyappSep	"^[ \t]*%}"

" eyapp comments
syn match eyappcomment          "#.*"
syn match eyappcomment         "\(\/\*\([^*]\|\*[^\\]\)*\*\/\)\{-1,}"

syn match	eyappCurlyError	"[{}]"

" semantic actions
syn region	eyappAction      matchgroup=eyappBlock start="{" end="}" contains=eyappAction,eyappKeyMethod,@perl 

syn region      eyappHeader    matchgroup=eyappCurly start=/^[ \t]*%{/ end=/^[ \t]*%}/  contains=@perl,eyappKeyMethod
syn region      eyappTail      matchgroup=eyappSectionSep start=/\(;[ \t\n]\+\)\@=%%/ end =/\%$/ contains=eyappKeyMethod,@perl 

syn match       eyappPerlSub  "\<sub\>\s\+"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_eyapp_syn_inits")
  if version < 508
    let did_eyapphdl_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " Internal eyapp highlighting links
  " HiLink eyappBrkt	Special
  HiLink eyappKey	Special
  HiLink eyappOper	Special
  HiLink eyappId	Special
  HiLink eyappString	Special
  HiLink eyappModifier	Special

  HiLink eyappUnionStart	eyappKey

  " External eyapp highlighting links
  HiLink eyappCurly	Delimiter
  HiLink eyappBlock	Normal
  HiLink eyappPerlSub	Function

  HiLink eyappCurlyError	Error
  HiLink eyappDefinition	Function
  HiLink eyappDelim	Function
  HiLink eyappKeyMethod	Special
  HiLink eyappSectionSep	Todo
  HiLink eyappSep	Delimiter
  HiLink eyappStmt	Statement
  HiLink eyappType	Type
  HiLink eyappcomment  Comment

  " since Bram doesn't like my Delimiter :|
  HiLink Delimiter	Type

  delcommand HiLink
endif

let b:current_syntax = "eyapp"


