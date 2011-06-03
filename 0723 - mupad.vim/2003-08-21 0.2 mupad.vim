"
" Vim syntax file
" Language   :	Mupad
" Maintainer :	Fabio Stumbo f.stumbo@unife.it
" File type  :	*.mu (see :help filetype)
"
" History
"	20030821	fabio	0.2		Added definition syntax with semicolon checking
"								and made some clean up 
"	20030731	fabio	0.1		Creation, copying here and there... ;-)
"
"	Comments, suggestions and remarks are welcome.


syntax clear

if version < 600
  set iskeyword=$,48-57,_,:,a-z,@-Z
else
  setlocal iskeyword=$,48-57,_,:,a-z,@-Z
endif

" parenthesis/curly/brace sanity checker
syn region mupadZone	matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" transparent contains=ALLBUT,mupadError,mupadBraceError,mupadCurlyError
syn region mupadZone	matchgroup=Delimiter start="{" matchgroup=Delimiter end="}" transparent contains=ALLBUT,mupadError,mupadBraceError,mupadParenError
syn region mupadZone	matchgroup=Delimiter start="\[" matchgroup=Delimiter end="]" transparent contains=ALLBUT,mupadError,mupadCurlyError,mupadParenError
syn match mupadError				"[)\]}]"
syn match mupadBraceError			"[)}]"	contained
syn match mupadCurlyError			"[)\]]"	contained
syn match mupadParenError			"[\]}]"	contained
syn match mupadComma				"[,;:]"
syn match mupadSemiError			"[;]"	contained

" Statement
" Split into booleans, conditionals, operators, repeat-logic, etc
syn keyword mupadFunction			proc begin local 
syn keyword	mupadFunction			end_proc:
syn keyword mupadCond				for if while then elif else	
syn keyword mupadCond				in to downto step 
syn keyword mupadCond				do from break
syn keyword	mupadCond				end_for: end_if: end_while: 
syn keyword mupadBool				TRUE FALSE UNKNOWN

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

" Definitions (with checking of ending semicolon)
syn match mupadAssign				":=" contains=mupadDefError,mupadDefProc
syn match mupadDefError				":=\([^;]*$\)\@="  contained
syn match mupadDefProc				":=[ \t]*proc"  contained

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

	HiLink	mupadArithmetic				Statement
	HiLink	mupadAssign					Statement
	HiLink	mupadBoolean				Constant
 	HiLink 	mupadBraceError				Error
	HiLink	mupadBuiltinFunction		Statement
  	HiLink 	mupadComma					Special
	HiLink	mupadComment				Comment
	HiLink	mupadComparison				Statement
	HiLink	mupadCond					Type
	HiLink	mupadConstant				Constant
  	HiLink 	mupadCurlyError				Error
  	HiLink 	mupadDefError				Error
  	HiLink 	mupadDefProc				Type
	HiLink	mupadError					Error
	HiLink	mupadFunction			 	PreProc
	HiLink	mupadNumber					Constant
  	HiLink 	mupadParenError				Error
	HiLink	mupadRepeat					Statement
  	HiLink 	mupadSemiError				Error
	HiLink	mupadString					Constant

delcommand HiLink
endif

let b:current_syntax = "mupad"

"EOF	vim: ts=4 noet tw=100 sw=4 sts=0
