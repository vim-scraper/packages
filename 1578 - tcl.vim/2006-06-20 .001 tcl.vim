" Vim syntax file
" Language:	TCL/TK
" Maintainer:	Dean Copsey <copsey@cs.ucdavis.edu>
"		(previously Matt Neumann <mattneu@purpleturtle.com>)
"		(previously Allan Kelly <allan@fruitloaf.co.uk>)
" Original:	Robin Becker <robin@jessikat.demon.co.uk>
" Last Change:	2004 May 16
"
" Keywords TODO: format clock click anchor

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" A bunch of useful keywords
syn match tclExternal   "\<\(package\)\s\(forget\|ifneeded\|names\|present\|provide\|require\|unknown\|vcompare\|versions\|satisfies\)\>"

syn keyword tclLabel		case default
syn keyword tclConditional  if then else elseif switch
syn keyword tclLooping		while for foreach break continue
syn keyword tclExceptions   catch error break return

syn keyword tclStatement    after append array auto_execok auto_import auto_load
syn keyword tclStatement    auto_mkindex auto_mkindex_old auto_qualify auto_reset
syn keyword tclStatement    bgerror binary cd clock close concat dde encoding eof
syn keyword tclStatement    eval exec exit expr fblocked fconfigure fcopy file
syn keyword tclStatement    fileevent flush format gets glob global history http
syn keyword tclStatement    incr info interp join lappend lindex linsert list
syn keyword tclStatement    llength load lrange lreplace lsearch lset lsort memory
syn keyword tclStatement    msgcat namespace open parray pid pkg::create pkg_mkIndex
syn keyword tclStatement    proc puts pwd read regexp registry regsub rename resource
syn keyword tclStatement    scan seek set socket source split string subst tcl_endOfWord
syn keyword tclStatement    tcl_findLibrary tcl_startOfNextWord tcl_startOfPreviousWord
syn keyword tclStatement    tcl_wordBreakAfter tcl_wordBreakBefore tell time trace
syn keyword tclStatement    unknown unset update uplevel upvar variable vwait

syn keyword tcltkStatement  bell bind bindtags button canvas checkbutton clipboard
syn keyword tcltkStatement  console destroy entry event focus font frame grab grid
syn keyword tcltkStatement  image label labelframe listbox lower menu menubutton
syn keyword tcltkStatement  message option pack panedwindow place radiobutton raise
syn keyword tcltkStatement  scale scrollbar selection send spinbox text tk tk_bisque
syn keyword tcltkStatement  tk_chooseColor tk_chooseDirectory tk_dialog tk_focusFollowsMouse
syn keyword tcltkStatement  tk_focusNext tk_focusPrev tk_getOpenFile tk_getSaveFile
syn keyword tcltkStatement  tk_menuSetFocus tk_messageBox tk_optionMenu tk_popup tk_setPalette
syn keyword tcltkStatement  tk_textCopy tk_textCut tk_textPaste tkerror tkwait toplevel
syn keyword tcltkStatement  winfo wm

syn keyword tcltkSwitch	contained	insert create polygon fill outline tag

" variable reference
	" ::optional::namespaces
syn match tclVarRef "$\(\(::\)\?\([[:alnum:]_.]*::\)*\)\a[a-zA-Z0-9_.]*"
	" ${...} may contain any character except '}'
syn match tclVarRef "${[^}]*}"

syn match tclOptionMatcher "\%(^\|\s\)\zs-\w\+" contains=tclOptionStarter
syn match tclOptionStarter contained "-"

" match commands - 2 lines for pretty match.
"variable
" Special case - If a number follows a variable region, it must be at the end of
" the pattern, by definition. Therefore, (1) either include a number as the region
" end and exclude tclNumber from the contains list, or (2) make variable
" keepend. As (1) would put variable out of step with everything else, use (2).
syn region tcltkCommand matchgroup=tcltkCommandColor start="^\<variable\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tclString,tclNumber,tclVarRef,tcltkCommand
syn region tcltkCommand matchgroup=tcltkCommandColor start="\s\<variable\>\|\[\<variable\>"hs=s+1 matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tclString,tclNumber,tclVarRef,tcltkCommand

" This isn't contained (I don't think) so it's OK to just associate with the Color group.
" TODO: This could be wrong.
syn keyword tcltkWidgetColor    toplevel

syn region tcltkPackConf matchgroup=tcltkPackConfColor start="\<configure\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkWidgetSwitch,tclString,tcltkSwitch,tcltkPackConfSwitch,tclNumber,tclVarRef keepend
syn region tcltkPackConf matchgroup=tcltkPackConfColor start="\<cget\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkWidgetSwitch,tclString,tcltkSwitch,tcltkPackConfSwitch,tclNumber,tclVarRef

" NAMESPACE
" commands associated with namespace
syn keyword tcltkNamespaceSwitch contained children code current delete eval
syn keyword tcltkNamespaceSwitch contained export forget import inscope origin
syn keyword tcltkNamespaceSwitch contained parent qualifiers tail which command variable
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<namespace\>" matchgroup=NONE skip="^\s*$" end="{\|}\|]\|\"\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkNamespaceSwitch

" EXPR
" commands associated with expr
syn keyword tcltkMaths	contained	acos	cos	hypot	sinh
syn keyword tcltkMaths	contained	asin	cosh	log	sqrt
syn keyword tcltkMaths	contained	atan	exp	log10	tan
syn keyword tcltkMaths	contained	atan2	floor	pow	tanh
syn keyword tcltkMaths	contained	ceil	fmod	sin
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<expr\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkMaths,tclNumber,tclVarRef,tclString,tcltlWidgetSwitch,tcltkCommand,tcltkPackConf

" format
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<format\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkMaths,tclNumber,tclVarRef,tclString,tcltlWidgetSwitch,tcltkCommand,tcltkPackConf

" PACK
" commands associated with pack
syn keyword tcltkPackSwitch	contained	forget info propogate slaves
syn keyword tcltkPackConfSwitch	contained	after anchor before expand fill in ipadx ipady padx pady side
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<pack\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkPackSwitch,tcltkPackConf,tcltkPackConfSwitch,tclNumber,tclVarRef,tclString,tcltkCommand keepend

" STRING
" commands associated with string
syn keyword tcltkStringSwitch	contained	compare first index last length match range tolower toupper trim trimleft trimright wordstart wordend
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<string\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkStringSwitch,tclNumber,tclVarRef,tclString,tcltkCommand

" ARRAY
" commands associated with array
syn keyword tcltkArraySwitch	contained	anymore donesearch exists get names nextelement size startsearch set
" match from command name to ] or EOL
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<array\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkArraySwitch,tclNumber,tclVarRef,tclString,tcltkCommand

" LSORT
" switches for lsort
syn keyword tcltkLsortSwitch	contained	ascii dictionary integer real command increasing decreasing index

" match from command name to ] or EOL
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<lsort\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkLsortSwitch,tclNumber,tclVarRef,tclString,tcltkCommand

syn keyword tclTodo contained	TODO

" String and Character contstants
" Highlight special characters (those which have a backslash) differently
syn match   tclSpecial contained "\\\d\d\d\=\|\\."

" A string needs the skip argument as it may legitimately contain \".
" Match at start of line
syn region  tclString   start=+^"+ end=+"+ contains=tclSpecial skip=+\\\\\|\\"+

"Match all other legal strings.
syn region  tclString	start=+[^\\]"+ms=s+1  end=+"+ contains=tclSpecial skip=+\\\\\|\\"+

syn match  tclLineContinue "\\\s*$"

"integer number, or floating point number without a dot and with "f".
syn case ignore

syn match  tclNumber	"\<\d\+\(u\=l\=\|lu\|f\)\>"

"floating point number, with dot, optional exponent
syn match  tclNumber		"\<\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\=\>"

"floating point number, starting with a dot, optional exponent
syn match  tclNumber		"\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"

"floating point number, without dot, with exponent
syn match  tclNumber	"\<\d\+e[-+]\=\d\+[fl]\=\>"

"hex number
syn match  tclNumber	"0x[0-9a-f]\+\(u\=l\=\|lu\)\>"

"syn match  tclIdentifier	"\<[a-z_][a-z0-9_]*\>"
syn case match

syn region tclComment	start="^\s*\#" skip="\\$" end="$" contains=tclTodo
syn region tclComment	start=/;\s*\#/hs=s+1 skip="\\$" end="$" contains=tclTodo

"syn sync ccomment tclComment

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_tcl_syntax_inits")
    if version < 508
        let did_tcl_syntax_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif

    HiLink tcltkSwitch		    Special
    HiLink tclLabel		        Label
    HiLink tclConditional		Conditional
    HiLink tclLooping		    Repeat
    HiLink tclNumber		    Number
    HiLink tclError		        Error
    HiLink tclStatement		    Statement

    "HiLink tclStatementColor	Statement

    HiLink tclString		    String
    HiLink tclComment		    Comment
    HiLink tclSpecial		    Special
    HiLink tclTodo		        Todo

    " Below here are the commands and their options.

    HiLink tcltkCommandColor	Statement
    HiLink tcltkWidgetColor	    Structure
    HiLink tclLineContinue	    WarningMsg
    HiLink tcltkStringSwitch	Special
    HiLink tcltkArraySwitch	    Special
    HiLink tcltkLsortSwitch	    Special
    HiLink tcltkPackSwitch	    Special
    HiLink tcltkPackConfSwitch	Special
    HiLink tcltkMaths		    Special
    HiLink tcltkNamespaceSwitch	Special
    HiLink tcltkWidgetSwitch	Special
    HiLink tcltkPackConfColor	Identifier
    HiLink tcltkStatement       Statement

    "HiLink tcltkLsort		    Statement

    HiLink tclVarRef		    Identifier
    HiLink tclOptionMatcher     Underlined
    HiLink tclExceptions        Exception
    HiLink tclExternal          Include

    delcommand HiLink
endif

let b:current_syntax = "tcl"

" vim: ts=4 sw=4 et
