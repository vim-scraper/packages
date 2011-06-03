" Vim syntax file
" Language:     Mathematica
" Maintainer:   steve <layland@wolfram.com>
" Last Change:  Sat Apr 30 14:23:38 CDT 2005
" Source:       http://members.wri.com/layland/vim/syntax/mma.vim
"
" Installation: 
" Unix users:
"   Drop this file in $HOME/.vim/syntax/mma.vim
"   and open a mathematica file. 
" 
" Windows users:
"   Ditto except in %HOME%\vimfiles\syntax\mma.vim
"
" NOTE:
" .m files are currently registered for Matlab.  If you would like
" to have .m files be associated with Mathematica, put the following
" auto command in your $HOME/.vim/filetype.vim file
"
"   au! BufRead,BufNewFile *.m      set ft=mma
" 
" I also recommend setting the default 'Comment' hilighting to something
" other than the color used for 'Function', since both are plentiful in
" most mathematica files, and they are often the same color (when using 
" background=dark).  I use
"
"   hi Comment ctermfg=darkcyan
"   
" darkgreen also looks good on my terminal.
"
" Credits:
" o  Original Mathematica syntax version written by
"    Wolfgang Waltenberger <wwalten@ben.tuwien.ac.at>
" o  Some ideas like the CommentStar,CommentTitle were adapted
"    from the Java vim syntax file by Claudio Fleiner.  Thanks!
" o  Everything else by steve <layland@wolfram.com>


if version < 600
	syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Group Definitions:
syntax cluster mmaNotes contains=mmaTodo,mmaFixme
syntax cluster mmaComments contains=mmaComment,mmaFunctionComment,mmaItem,mmaFunctionTitle,mmaCommentStar
syntax cluster mmaStrings contains=mmaLooseQuote,mmaCommentString,mmaUnicode
syntax cluster mmaTop contains=mmaOperator,mmaGenericFunction,mmaPureFunction,mmaVariable


" Comment Keywords:
syntax keyword mmaTodo TODO NOTE HEY contained
syntax match mmaTodo "X\{3,}" contained
syntax keyword mmaFixme FIX[ME] FIXTHIS BROKEN contained
" yay pirates...
syntax match mmaFixme "\%(Y\=A\+R\+G\+\|GRR\+\|CR\+A\+P\+\)\%(!\+\)\=" contained
syntax match mmaemPHAsis "\(_\+\)\%(\1\@!.\)\+\1" contained

" Comment Sections:
"   this:
"   :that:
syntax match mmaItem "\%(^[( |*\t]*\)\@<=\%(:\+\|\a\)[a-zA-Z0-9 ]\+:" contained contains=@mmaNotes

" Actual Mathematica Comments:
"   (* *)
"   allow nesting (* (* *) *) even though the frontend
"   won't always like it.
syntax region mmaComment start=+(\*+ end=+\*)+ skipempty contains=@mmaNotes,mmaItem,@mmaStrings,mmaemPHAsis,mmaComment,mmaCommentString

" Function Comments:
"   just like a normal comment except the first sentance is Special ala Java
"    (** **)
syntax region mmaFunctionComment start="(\*\*\+" end="\*\+)" contains=@mmaNotes,mmaItem,mmaFunctionTitle,@mmaStrings,mmaemPHAsis,mmaComment
syntax region mmaFunctionTitle contained matchgroup=mmaFunctionComment start="\%((\*\*[ *]*\)" matchgroup=mmaFunctionTitle keepend end="\w[.!-]\=\s*$" end="[.!-][ \t\r<&]"me=e-1 end="\%(\*\+)\)\@=" contained contains=@mmaNotes,mmaItem,mmaCommentStar

" catch remaining (**********)'s
syntax match mmaComment "(\*\*\+)"
" catch preceding *
syntax match mmaCommentStar "^\s*\*\+" contained

" Variables:
"   Dollar sign variables
syntax match mmaVariable "$\a\+\d*"
"   Preceding contexts
syntax match mmaVariable "`\=\a\+\d*`"

" Numbers:
syntax match mmaNumber "\<\%(\d\+\.\=\d*\|\d*\.\=\d\+\)\>"
syntax match mmaNumber "`\d\+\>"

" Predefined Constants:
"   to list all predefined Symbols would be too insane...
"   it's probably smarter to define a select few, and get the rest from
"   context if absolutely necessary.
"   TODO - populate this with other often used Symbols

" standard fixed symbols:
syntax keyword mmaVariable True False None Automatic All Null C General

" mathematical constants:
syntax keyword mmaVariable Pi I E Infinity ComplexInfinity Indeterminate GoldenRatio EulerGamma Degree Catalan Khinchin Glaisher 

" stream data / atomic heads:
syntax keyword mmaVariable Byte Character Expression Number Real String Word EndOfFile Integer Symbol

" sets:
syntax keyword mmaVariable Integers Complexes Reals Booleans Rationals

" character classes:
syntax keyword mmaPattern DigitCharacter LetterCharacter WhitespaceCharacter WordCharacter EndOfString StartOfString EndOfLine StartOfLine WordBoundary

" SelectionMove directions/units:
syntax keyword mmaVariable Next Previous After Before Character Word Expression TextLine CellContents Cell CellGroup EvaluationCell ButtonCell GeneratedCell Notebook
syntax keyword mmaVariable CellTags CellStyle CellLabel

" TableForm positions:
syntax keyword mmaVariable Above Below Left Right

" colors:
syntax keyword mmaVariable Black Blue Brown Cyan Gray Green Magenta Orange Pink Purple Red White Yellow

" function attributes
syntax keyword mmaVariable Protected Listable OneIdentity Orderless Flat Constant NumericFunction Locked ReadProtected HoldFirst HoldRest HoldAll HoldAllComplete SequenceHold NHoldFirst NHoldRest NHoldAll Temporary Stub 

" Strings:
"   "string"
"   'string' is not accepted (until literal strings are supported!)
syntax region mmaString start=+\\\@<!"+ skip=+\\\@<!\\\%(\\\\\)*"+ end=+"+
syntax region mmaCommentString oneline start=+\\\@<!"+ skip=+\\\@<!\\\%(\\\\\)*"+ end=+"+ contained

" Patterns:
"   Each pattern marker below can be Blank[] (_), BlankSequence[] (__)
"   or BlankNullSequence[] (___).  Most examples below can also be 
"   combined, for example Pattern tests with Default values.
"   
"   _Head                   Anonymous patterns
"   name_Head 
"   name:(_Head|_Head2)     Named patterns
"    
"   _Head : val
"   name:_Head:val          Default values
"
"   _Head?testQ, 
"   _Head?(test[#]&)        Pattern tests
"
"   name_Head/;test[name]   Conditionals
"   
"   _Head:.                 Predefined Default
"
"   .. ...                  Pattern Repeat
   
syntax match mmaPatternError "\%(_\{4,}\|)\s*&\s*)\@!\)" contained

"pattern name:
syntax match mmaPattern "[A-Za-z0-9`]\+\s*:\+[=>]\@!" contains=mmaOperator
"pattern default:
syntax match mmaPattern ": *[^ ,]\+[\], ]\@=" contains=@mmaStrings,@mmaTop,mmaOperator
"pattern head:
syntax match mmaPattern "[A-Za-z0-9`]*_\+\%(\a\+\)\=\%(?([^)]\+)\|?[^\],]\+\)\=" contains=@mmaTop,@mmaStrings,mmaPatternError

" Function Usage Messages:
"   "SymbolName::item"
syntax match mmaMessage "$\=\a\+\d*::\a\+\d*"

" Operators:
"   /: ^= ^:=   UpValue
"   /;          Conditional
"   := =        DownValue
"   == === ||
"   != =!= &&   Logic
"   >= <= < >
"   += -= *=
"   /= ++ --    Math
"   ^* 
"   -> :>       Rules
"   @@ @@@      Apply
"   /@ //@      Map
"   /. //.      Replace
"   // @        Function application
"   <> ~~       String/Pattern join
"   ~           infix operator
"   . :         Pattern operators
syntax match mmaOperator "\%(@\{1,3}\|//[.@]\=\)"
syntax match mmaOperator "\%(/[;:@.]\=\|\^\=:\==\)"
syntax match mmaOperator "\%([-:=]\=>\|<=\=\)"
"syntax match mmaOperator "\%(++\=\|--\=\|[/+-*]=\|[^*]\)"
syntax match mmaOperator "[*+=^.:?-]"
syntax match mmaOperator "\%(\~\~\=\)" containedin=ALLBUT,@mmaComments,@mmaStrings
syntax match mmaOperator "\%(=\{2,3}\|=\=!=\|||\=\|&&\|!\)" contains=ALLBUT,mmaPureFunction

" Pure Functions:
syntax match mmaPureFunction "#\%(#\|\d\+\)\="
syntax match mmaPureFunction "&"

" Named Functions:
" Since everything is pretty much a function, get this straight 
" from context

syntax match mmaGenericFunction "[A-Za-z0-9`]\+\s*\%([@[]\|/:\)\@=" contains=mmaOperator
syntax match mmaGenericFunction "\~\s*[^~]\+\s*\~"hs=s+1,he=e-1 contains=mmaOperator,mmaBoring
syntax match mmaGenericFunction "//\s*[A-Za-z0-9`]\+"hs=s+2 contains=mmaOperator
   
" Special Characters:
"   \[Name]     named character
"   \ooo        octal
"   \.xx        2 digit hex
"   \:xxxx      4 digit hex (multibyte unicode)
syntax match mmaUnicode "\\\[\w\+\d*\]"
syntax match mmaUnicode "\\\%(\x\{3}\|\.\x\{2}\|:\x\{4}\)"

" Syntax Errors:
syntax match mmaError "\*)" containedin=ALLBUT,@mmaComments,@mmaStrings
syntax match mmaError "\%([&:|+*/?-]\{3,}\|[.=]\{4,}\|_\@<=\.\{2,}\|`\{2,}\)" containedin=ALLBUT,@mmaComments,@mmaStrings


" Punctuation:
" things that shouldn't really be highlighted, or highlighted 
" in they're own group if you _really_ want. :)
"  ( ) { }
syntax match mmaBoring "[(){}]" contained

" Function Arguments:
"   anything between brackets []
"   TODO - make good folds for this.
"   this is useful mainly for folding since almost _everything_ is 
"   a function argument in a functional language... ;)
"syntax region mmaArgument start="\[" end="]" containedin=ALLBUT,@mmaComments,@mmaStrings transparent fold
"syntax sync fromstart
"set foldmethod=syntax
"set foldminlines=10

if version >= 508 || !exists("did_mma_syn_inits")
	if version < 508
		let did_mma_syn_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

    HiLink mmaComment           Comment
    HiLink mmaCommentStar       Comment
    HiLink mmaFunctionComment   Comment
    HiLink mmaLooseQuote        Comment
	HiLink mmaOperator          Operator
    HiLink mmaPatternOp         Operator
	HiLink mmaPureFunction      Operator
	HiLink mmaVariable          Identifier
	HiLink mmaString            String
    HiLink mmaCommentString     String
	HiLink mmaUnicode           String
	HiLink mmaMessage           Type
	HiLink mmaPattern           Type
	HiLink mmaGenericFunction   Function
	HiLink mmaError             Error
	HiLink mmaFixme             Error
    HiLink mmaPatternError      Error
    HiLink mmaTodo              Todo
    HiLink mmaNumber            Type
    HiLink mmaemPHAsis          Special
    HiLink mmaFunctionTitle     Special
    HiLink mmaItem              Preproc

	delcommand HiLink
endif

let b:current_syntax = "mma"
