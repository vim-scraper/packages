"
" Vim syntax file
" Language   :	Mupad
" Maintainer :	Fabio Stumbo f.stumbo@unife.it
" File type  :	*.mu (see :help filetype)
"
" History
"	20030731	fabio		0.1		Creation, copying here and there... ;-)
"
"	Comments, suggestions and remarks are welcome.


syntax clear

if version < 600
  set iskeyword=$,48-57,_,a-z,@-Z
else
  setlocal iskeyword=$,48-57,_,a-z,@-Z
endif



" parenthesis/curly/brace sanity checker
syn region mupadZone	matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" transparent contains=ALLBUT,mupadError,mupadBraceError,mupadCurlyError
syn region mupadZone	matchgroup=Delimiter start="{" matchgroup=Delimiter end="}" transparent contains=ALLBUT,mupadError,mupadBraceError,mupadParenError
syn region mupadZone	matchgroup=Delimiter start="\[" matchgroup=Delimiter end="]" transparent contains=ALLBUT,mupadError,mupadCurlyError,mupadParenError
syn match  mupadError				"[)\]}]"
syn match  mupadBraceError			"[)}]"	contained
syn match  mupadCurlyError			"[)\]]"	contained
syn match  mupadParenError			"[\]}]"	contained
syn match  mupadComma				"[,;:]"
syn match  mupadSemiError			"[;:]"	contained


" Statement
" Split into booleans, conditionals, operators, repeat-logic, etc
syn keyword mupadFunction			proc end_proc begin local
syn keyword mupadBool				TRUE FALSE UNKNOWN
syn keyword mupadIfFor				for end_for if end_if while end_while
syn keyword mupadCond				then elif 	
syn keyword mupadRepeat				in to downto step 
syn keyword mupadRepeat				do from 
syn keyword mupadStatement			break	

" Builtin constants and functions
syn match mupadConstant				"complexInfinity" "infinity" "E" "exp(1)" "EULER" "PI"
syn match mupadBuiltinFunction		"abs" "arg" "bernouli" "besselI" "besselJ" "besselK"
syn match mupadBuiltinFunction		"besselY" "beta" "binomial" "ceil" "Ci" "dilog"
syn match mupadBuiltinFunction		"dirac" "Ei" "erf" "erfc" "exp" "fact" "floor" "gamma"
syn match mupadBuiltinFunction		"heaviside" "hypergeom" "igamma" "Im" "lambertV"
syn match mupadBuiltinFunction		"lambertW" "log" "ln" "max" "min" "polylog" "psi"
syn match mupadBuiltinFunction		"Re" "round" "Si" "sign" "sqrt" "trunc" "zeta"
syn match mupadBuiltinFunction		"cos" "cot" "csc" "sec" "sin" "tan" "cosh" "coth" 
syn match mupadBuiltinFunction		"csch" "sech" "sinh" "tanh"
syn match mupadBuiltinFunction		"arccos" "arccot" "arccsc" "arcsec" "arcsin" "arctan" 
syn match mupadBuiltinFunction		"arccosh" "arccoth" 
syn match mupadBuiltinFunction		"arccsch" "arcsech" "arcsinh" "arctanh"
syn match mupadBoolean				"and" "or" "not" "xor" "==>" "<=>"


" Delimiters and operators.
syn match mupadComparison			"[=~]="
syn match mupadComparison			"[<>]="
syn match mupadComparison			"<>"
syn match mupadVarAssign			"[ \t]*:=[ \t]*" contains=mupadAssign
syn match mupadAssign				":="	contained
syn match mupadArithmetic			"[+-]"
syn match mupadArithmetic			"\.\=[*/\\]\.\="
syn match mupadArithmetic			"\.\=^"


"" Comments and tools.
syn region mupadComment				start="\#" end="\#"  
syn region mupadComment				start="//" end="$" oneline
syn region mupadComment				start="/\*" end="\*/"  

" Constants.
syn match mupadNumber				"[0-9]\+\(\.[0-9]*\)\=\([DEde][+-]\=[0-9]\+\)\="
syn match mupadNumber				"\.[0-9]\+\([DEde][+-]\=[0-9]\+\)\="
syn region mupadString				start=+"+ end=+"+				oneline



" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_maplev_syntax_inits")
  if version < 508
    let did_maplev_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

	HiLink	mupadArithmetic				Operator
	HiLink	mupadAssign					Operator
	HiLink	mupadBoolean				Boolean
 	HiLink 	mupadBraceError				mupadError
	HiLink	mupadBuiltinFunction		Keyword
  	HiLink 	mupadComma					Delimiter
	HiLink	mupadComment				Comment
	HiLink	mupadComparison				Operator
	HiLink	mupadConstant				Constant
	HiLink	mupadCond					Conditional
  	HiLink 	mupadCurlyError				mupadError
	HiLink	mupadError					Error
	HiLink	mupadFunction			 	Type
	HiLink	mupadIfFor					Underlined
	HiLink	mupadNumber					Number
  	HiLink 	mupadParenError				mupadError
	HiLink	mupadRepeat					Repeat
  	HiLink 	mupadSemiError				mupadError
	HiLink	mupadStatement				Statement
	HiLink	mupadString					String

delcommand HiLink
endif

let b:current_syntax = "mupad"

"EOF	vim: ts=4 noet tw=100 sw=4 sts=0
