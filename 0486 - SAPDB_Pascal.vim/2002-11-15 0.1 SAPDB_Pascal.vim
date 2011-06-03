" Vim syntax file
" Language:	Pascal (SAPDB)
" Maintainer:	Gert Groﬂmann <gert.grossmann@sap.com>
" Last change:	1997 April 25

syn clear
syn case ignore

syn keyword pascalStatement		program begin end
syn keyword pascalLabel		        case goto otherwise
syn keyword pascalConditional	        if else then
syn keyword pascalRepeat		do for while to until repeat

syn match   pascalType /[^.]\<t\(ak\|gg\|bd\|kb\|ta\|sp\|ut\)\(\d\d*\)\=_[^_][^;( ]\+\>/
syn keyword pascalType	        char const integer real text
syn keyword pascalType	        var type string
syn match pascalType	/\<SAPDB_\w\+\>/

syn match  pascalMatrixDelimiter	"[][]"
syn match  pascalNumber		"-\=\<\d\+\>"
syn match  pascalByte		"\$[0-9a-fA-F]\+\>"

syn region pascalComment	start="(\*"  end="\*)" contains=pascalTodo

syn keyword pascalOperator	and array boolean div downto
syn keyword pascalOperator	true false in 
syn keyword pascalOperator	mod nil not of or packed
syn keyword pascalOperator	record set with

syn keyword pascalFunction	procedure function

syn match  pascalPreProc   "^&\s*\(ifdef\|ifndef\|else\|endif\)\(.*\)\="
syn region Frame matchgroup=NONE keepend start=/^Specification\|^Description\s*:/ end=/^\.CM\s*-lll-\s*$/ 
syn region pascalTrace	start="^&\s*ifdef\s\+trace" end="^&\s*\(endif\|else\)" contains=pascalComment
syn region  pascalString	start=+'+  end=+'+ oneline

syn match pascalProc /[a-z]\w\+\_s*([^\*]/me=e-2
syn match pascalProc /[a-z]\w\+\_s*($/me=e-1

syn sync lines=450

if !exists("did_sapdbpascal_syntax_inits")
  let did_sapdbpascal_syntax_inits = 1
  hi link pascalStatement		Statement
  hi link pascalLabel			Label
  hi link pascalConditional		Conditional
  hi link pascalRepeat			Repeat
  hi link pascalTodo			Todo
  hi link pascalString			String
  hi link pascalMatrixDelimiter		Identifier
  hi link pascalNumber			Number
  hi link pascalByte			Number
  hi link pascalOperator		Operator
  hi link pascalFunction		Function
  hi link pascalType			Type
  hi link pascalComment		        Comment
  hi link pascalStatement		Statement
  hi link pascalTrace		        PreProc
  hi link pascalPreProc		        PreProc
  hi link pascalProc                    VisualNOS
  hi link Frame                         NONE
endif

let b:current_syntax = "sapdb_pascal"
" vim: ts=8
