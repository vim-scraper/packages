" Vim syntax file
" Language:	(g)pic with m4 macros
" Maintainer:	Michael Muhler <muhler AT web.de>
" Last Change:	Wed Apr 15 17:44:06 2003
" URL:          http://home.arcor.de/muhler

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" we'll try to use the naming conventions
" suggested in syntax.txt
syn match m4picComment	      "[%#].*$"	contains=m4picTodo,m4picTab

" parenthesis/curly/brace sanity checker, from maple.vim
syn region mvZone	matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" transparent contains=ALLBUT,mvError,mvBraceError,mvCurlyError
syn region mvZone	matchgroup=Delimiter start="{" matchgroup=Delimiter end="}" transparent contains=ALLBUT,mvError,mvBraceError,mvParenError
syn region mvZone	matchgroup=Delimiter start="\[" matchgroup=Delimiter end="]" transparent contains=ALLBUT,mvError,mvCurlyError,mvParenError
syn match  mvError		"[)\]}]"
syn match  mvBraceError	"[)}]"	contained
syn match  mvCurlyError	"[)\]]"	contained
syn match  mvParenError	"[\]}]"	contained



syn keyword m4picInclude                include cct_init sinclude s_init
syn keyword m4picPreProc                define ifdef
syn keyword m4picStatement	        return
syn keyword m4picConditional		else elseif end if otherwise try catch
syn keyword m4picRepeat	        	do for while by

syn keyword m4picSpecial                clc clear global pause

syn keyword m4picDirCommand             left right up down
syn keyword m4picDirCommand             left_ right_ up_ down_
syn keyword m4picDirCommand             from to and at between
syn keyword m4picDirCommand             then with last
syn keyword m4picTodo			contained  TODO

" If you do not want these operators lit, uncommment them and the "hi link" below
syn match m4picArithmeticOperator	"[-+]"
syn match m4picArithmeticOperator	"\.\=[*/\\^]"
syn match m4picRelationalOperator	"[=~]="
syn match m4picRelationalOperator	"[<>]=\="
syn match m4picLogicalOperator		"[&|~]"

syn match m4picLineContinuation	"\.\{3}"

" match objects
syn match m4picObject		"[A-Z][a-zA-Z_0-9]*:"
syn keyword m4picObject               Here last

" String
syn region m4picString			start=+"+ end=+"+	oneline

" If you don't like tabs
syn match m4picTab			"\t"

" Standard numbers
syn match m4picNumber		"\<\d\+[ij]\=\>"
" Special number
syn keyword m4picNumber        inf NaN pi         
" the complex number is excluded
" floating point number, with dot, optional exponent
syn match m4picFloat		"\<\d\+\(\.\d*\)\=\([edED][-+]\=\d\+\)\=[ij]\=\>"
" floating point number, starting with a dot, optional exponent
syn match m4picFloat		"\.\d\+\([edED][-+]\=\d\+\)\=[ij]\=\>"

" Transpose character and delimiters: Either use just [...] or (...) aswell
syn match m4picTransposeOperator	"[])a-zA-Z0-9.]'"lc=1

syn match m4picSemicolon		";"

syn keyword m4picLengths               wid ht scale rad
syn keyword m4picVariable              arcrad arrowht arrowwid boxht boxrad
syn keyword m4picVariable              boxwid circlerad dashwid ellipseht 
syn keyword m4picVariable              ellipsewid lineht linewid moveht movewid
syn keyword m4picVariable              textht textwid maxpsht maxpswid
syn keyword m4picVariable              thinlines_ thicklines_ dimen_
syn keyword m4picVariable              linethick_ dimension_ fillval


"syn keyword m4picFunction	       
syn keyword m4picCircuitMacros	       AND_gate BUFFER_gate FlipFlop NAND_gate 
syn keyword m4picCircuitMacros	       NOR_gate NOT_gate NXOR_gate OR_gate
syn keyword m4picCircuitMacros	       XOR_gate amp battery bi_tr capacitor
syn keyword m4picCircuitMacros	       cross cross3D d_fet delay diode dot dot3D
syn keyword m4picCircuitMacros	       e_fet ebox fuse gpar_ ground hop inductor
syn keyword m4picCircuitMacros	       integrator j_fet mosfet opamp relay
syn keyword m4picCircuitMacros	       resistor scr sfgnode smosfet source switch
syn keyword m4picCircuitMacros	       transformer ujt xtal b_current contact
syn keyword m4picCircuitMacros	       Point_ vec_ rvec_

syn match m4picKeyword   	       ".PS"
syn match m4picKeyword   	       ".PE"

" basic commands from pic
syn keyword m4picCommand               arrow line spline move
syn keyword m4picCommand               box circle ellipse arc
" we will include the label command here since they produce something
syn keyword m4picCommand               llabel rlabel dlabel
syn keyword m4picCommand               ljust rjust above below

" functions from elfun (elementary math functions)
syn keyword m4picImplicit       sin sinh asin asinh cos cosh acos acosh	
syn keyword m4picImplicit       Sin sinh asin asinh Cos cosh acos acosh	

syn match m4picError	"-\=\<\d\+\.\d\+\.[^*/\\^]"
syn match m4picError	"-\=\<\d\+\.\d\+[eEdD][-+]\=\d\+\.\([^*/\\^]\)"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_m4pic_syntax_inits")
  if version < 508
    let did_m4pic_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink m4picImplicit		        m4picStatement
  HiLink m4picTransposeOperator	        m4picOperator
  HiLink m4picArithmeticOperator	m4picOperator
  HiLink m4picRelationalOperator	m4picOperator
  HiLink m4picLogicalOperator		m4picOperator
 
" link m4pic groups to standard highlight links
  
  HiLink m4picKeyword                   Keyword
  HiLink m4picOperator		        Operator
  HiLink m4picLineContinuation	        Special
  HiLink m4picConditional		Conditional
  HiLink m4picRepeat			Repeat
  HiLink m4picTodo			Todo
  HiLink m4picString			String
  HiLink m4picDelimiter		        Delimiter
  HiLink m4picNumber			Number
  HiLink m4picFloat			Float
  HiLink m4picCircuitMacros	        Function
  HiLink m4picFunction		        Function
  HiLink m4picError			Error
  HiLink m4picCommand		        Statement
  HiLink m4picStatement		        Statement
  HiLink m4picVariable                  Special
  HiLink m4picLengths                   Special
  HiLink m4picSpecial                   Special
  HiLink m4picSemicolon		        SpecialChar
  HiLink m4picComment			Comment
  HiLink m4picDirCommand                Label 
  HiLink m4picInclude                   Include 
  HiLink m4picPreProc                   PreProc 
  HiLink m4picObject     		Tag

  delcommand HiLink
endif

let b:current_syntax = "m4pic"

"EOF	vim: ts=8 noet tw=100 sw=8 sts=0
