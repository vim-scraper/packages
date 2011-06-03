" Vim syntax file
" Language:	JIAC Agent Description Language
" Maintainer:	Marco Kunze <makunze@cs.tu-berlin.de>
" Filenames:    *.jadl; *.onto
" Last Change:	1st December 2005
" Web Page:     none
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

syn keyword jadlTodo contained	TODO FIXME XXX

" JADL keywords
syn keyword jadlKeyword ont package import include 
syn keyword jadlKeyword act script prot pre eff var seq branch cond 
syn keyword jadlKeyword obj service meta known goal eval objref fun 
syn keyword jadlKeyword fail cont inform send receive update ? add 
syn keyword jadlKeyword ipar alt end done #begincode #endcode #import 
syn keyword jadlKeyword #code bind unbind ontology iseq cat incl neede 
syn keyword jadlKeyword fixed defined const ext init default constr file 
syn keyword jadlKeyword private exec inference call primitive mprot user 
syn keyword jadlKeyword provider par ialt mseq mpar malt break remove 
syn keyword jadlKeyword loop logerror logwarn loginfo logdebug log case 
syn keyword jadlKeyword replytags def inf data reason fact rule role 
syn keyword jadlKeyword cast newobj new or and not att comp known 
syn keyword jadlKeyword unknown forall exists 

syn keyword jadlConstant unknown false null true

syn keyword jadlType agentname bool string int url class real timestamp abstract class


syn match jadlString "\"[^"]*\""

syn match jadlVariable "?[a-zA-Z0-9_]*"

syn region  jadlComment		 start="/\*"  end="\*/" 
syn match jadlComment	"//.*" contains=jadlTodo
  
if filereadable(expand("$VIMRUNTIME/syntax")."/java.vim")
  syn include @jadlJava $VIMRUNTIME/syntax/java.vim
  unlet b:current_syntax
  syn region java matchgroup=vimScriptDelim start="#begincode" end="#endcode" contains=@jadlJava
endif


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_jadl_syntax_inits")
  if version < 508
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink jadlTodo	Todo

  HiLink jadlKeyword		Keyword
  HiLink jadlConstant		SpecialChar

  HiLink jadlComment		Comment

  HiLink jadlType		Type

  HiLink jadlVariable		Identifier

  HiLink jadlString		String

  delcommand HiLink
endif

let b:current_syntax = "jadl"

" vim: ts=8
