" Vim syntaxtax file
" Language:	TCL/TK
" Maintainer:	Dean Copsey <copsey@cs.ucdavis.edu>
"		(previously Matt Neumann <mattneu@purpleturtle.com>)
"		(previously Allan Kelly <allan@fruitloaf.co.uk>)
" Original:	Robin Becker <robin@jessikat.demon.co.uk>
" Dev Version: Robert Hicks <sigzero@gmail.com> and Tomasz Kalkosinski
" Last Change:	2006 June 23
"
" Keywords TODO: format clock click anchor

" For version 5.x: Clear all syntaxtax items
" For version 6.x: Quit when a syntaxtax file was already loaded
if version < 600
    syntaxtax clear
elseif exists("b:current_syntaxtax")
    finish
endif

" A bunch of useful keywords
syntax match tclExternal   "\<\(package\)\s\(forget\|ifneeded\|names\|present\|provide\|require\|unknown\|vcompare\|versions\|satisfies\)\>"

syntax keyword tclLabel		case default
syntax keyword tclConditional  if then else elseif switch
syntax keyword tclLooping		while for foreach break continue
syntax keyword tclExceptions   catch error break return

" Things that I removed because they start one of regions below:
" variable namespace expr format string array lsort

syntax keyword tclStatement    after append auto_execok auto_import auto_load
syntax keyword tclStatement    auto_mkindex auto_mkindex_old auto_qualify auto_reset
syntax keyword tclStatement    bgerror binary cd clock close concat dde encoding eof
syntax keyword tclStatement    eval exec exit fblocked fconfigure fcopy file
syntax keyword tclStatement    fileevent flush gets glob global history http
syntax keyword tclStatement    incr info interp join lappend lindex linsert list
syntax keyword tclStatement    llength load lrange lreplace lsearch lset memory
syntax keyword tclStatement    msgcat open parray pid pkg::create pkg_mkIndex
syntax keyword tclStatement    proc puts pwd read regexp registry regsub rename resource
syntax keyword tclStatement    scan seek set socket source split subst tcl_endOfWord
syntax keyword tclStatement    tcl_findLibrary tcl_startOfNextWord tcl_startOfPreviousWord
syntax keyword tclStatement    tcl_wordBreakAfter tcl_wordBreakBefore tell time trace
syntax keyword tclStatement    unknown unset update uplevel upvar vwait

syntax keyword tcltkStatement  bell bind bindtags button canvas checkbutton clipboard
syntax keyword tcltkStatement  console destroy entry event focus font frame grab grid
syntax keyword tcltkStatement  image label labelframe listbox lower menu menubutton
syntax keyword tcltkStatement  message option pack panedwindow place radiobutton raise
syntax keyword tcltkStatement  scale scrollbar selection send spinbox text tk tk_bisque
syntax keyword tcltkStatement  tk_chooseColor tk_chooseDirectory tk_dialog tk_focusFollowsMouse
syntax keyword tcltkStatement  tk_focusNext tk_focusPrev tk_getOpenFile tk_getSaveFile
syntax keyword tcltkStatement  tk_menuSetFocus tk_messageBox tk_optionMenu tk_popup tk_setPalette
syntax keyword tcltkStatement  tk_textCopy tk_textCut tk_textPaste tkerror tkwait toplevel
syntax keyword tcltkStatement  winfo wm

syntax keyword tcltkSwitch	contained	insert create polygon fill outline tag

" WIDGETS
" commands associated with widgets
syntax keyword tcltkWidgetSwitch contained background highlightbackground insertontime cget
syntax keyword tcltkWidgetSwitch contained selectborderwidth borderwidth highlightcolor insertwidth
syntax keyword tcltkWidgetSwitch contained selectforeground cursor highlightthickness padx setgrid
syntax keyword tcltkWidgetSwitch contained exportselection insertbackground pady takefocus
syntax keyword tcltkWidgetSwitch contained font insertborderwidth relief xscrollcommand
syntax keyword tcltkWidgetSwitch contained foreground insertofftime selectbackground yscrollcommand
syntax keyword tcltkWidgetSwitch contained height spacing1 spacing2 spacing3
syntax keyword tcltkWidgetSwitch contained state tabs width wrap

" button
syntax keyword tcltkWidgetSwitch contained command default

" canvas
syntax keyword tcltkWidgetSwitch contained closeenough confine scrollregion xscrollincrement yscrollincrement orient

" checkbutton, radiobutton
syntax keyword tcltkWidgetSwitch contained indicatoron offvalue onvalue selectcolor selectimage state variable

" entry, frame
syntax keyword tcltkWidgetSwitch contained show class colormap container visual

" listbox, menu
syntax keyword tcltkWidgetSwitch contained selectmode postcommand selectcolor tearoff tearoffcommand title type

" menubutton, message
syntax keyword tcltkWidgetSwitch contained direction aspect justify

" scale
syntax keyword tcltkWidgetSwitch contained bigincrement digits from length resolution showvalue sliderlength sliderrelief tickinterval to

" scrollbar
syntax keyword tcltkWidgetSwitch contained activerelief elementborderwidth

" image
syntax keyword tcltkWidgetSwitch contained delete names types create

" menu, mane add
syntax keyword tcltkWidgetSwitch contained active end last none cascade checkbutton command radiobutton separator
syntax keyword tcltkWidgetSwitch contained activebackground actveforeground accelerator background bitmap columnbreak
syntax keyword tcltkWidgetSwitch contained font foreground hidemargin image indicatoron label menu offvalue onvalue
syntax keyword tcltkWidgetSwitch contained selectcolor selectimage state underline value variable
syntax keyword tcltkWidgetSwitch contained add clone configure delete entrycget entryconfigure index insert invoke
syntax keyword tcltkWidgetSwitch contained post postcascade type unpost yposition activate
syntax keyword tcltkWidgetSwitch contained
syntax match tcltkWidgetSwitch contained
syntax region tcltkWidget matchgroup=tcltkWidgetColor start="\<button\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1 contains=tclLineContinue,tcltkWidgetSwitch,tclString,tcltkSwitch,tclNumber,tclVarRef
syntax region tcltkWidget matchgroup=tcltkWidgetColor start="\<scale\>" matchgroup=NONE ski

" variable reference
	" ::optional::namespaces
syntax match tclVarRef "$\(\(::\)\?\([[:alnum:]_.]*::\)*\)\a[a-zA-Z0-9_.]*"
	" ${...} may contain any character except '}'
syntax match tclVarRef "${[^}]*}"

syntax match tclOptionMatcher "\%(^\|\s\)\zs-[A-Za-z]\+" contains=tclOptionStarter
syntax match tclOptionStarter contained "-"

" match commands - 2 lines for pretty match.
"variable
" Special case - If a number follows a variable region, it must be at the end of
" the pattern, by definition. Therefore, (1) either include a number as the region
" end and exclude tclNumber from the contains list, or (2) make variable
" keepend. As (1) would put variable out of step with everything else, use (2).
syntax region tcltkCommand matchgroup=tcltkCommandColor start="^\<variable\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tclString,tclNumber,tclVarRef,tcltkCommand
syntax region tcltkCommand matchgroup=tcltkCommandColor start="\s\<variable\>\|\[\<variable\>"hs=s+1 matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tclString,tclNumber,tclVarRef,tcltkCommand

" This isn't contained (I don't think) so it's OK to just associate with the Color group.
" TODO: This could be wrong.
syntax keyword tcltkWidgetColor    toplevel

syntax region tcltkPackConf matchgroup=tcltkPackConfColor start="\<configure\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkWidgetSwitch,tclString,tcltkSwitch,tcltkPackConfSwitch,tclNumber,tclVarRef keepend
syntax region tcltkPackConf matchgroup=tcltkPackConfColor start="\<cget\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkWidgetSwitch,tclString,tcltkSwitch,tcltkPackConfSwitch,tclNumber,tclVarRef

" NAMESPACE
" commands associated with namespace
syntax keyword tcltkNamespaceSwitch contained children code current delete eval
syntax keyword tcltkNamespaceSwitch contained export forget import inscope origin
syntax keyword tcltkNamespaceSwitch contained parent qualifiers tail which command variable
syntax region tcltkCommand matchgroup=tcltkCommandColor start="\<namespace\>" matchgroup=NONE skip="^\s*$" end="{\|}\|]\|\"\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkNamespaceSwitch

" EXPR
" commands associated with expr
syntax keyword tcltkMaths	contained	acos	cos	hypot	sinh
syntax keyword tcltkMaths	contained	asin	cosh	log	sqrt
syntax keyword tcltkMaths	contained	atan	exp	log10	tan
syntax keyword tcltkMaths	contained	atan2	floor	pow	tanh
syntax keyword tcltkMaths	contained	ceil	fmod	sin
syntax region tcltkCommand matchgroup=tcltkCommandColor start="\<expr\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkMaths,tclNumber,tclVarRef,tclString,tcltlWidgetSwitch,tcltkCommand,tcltkPackConf

" format
syntax region tcltkCommand matchgroup=tcltkCommandColor start="\<format\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkMaths,tclNumber,tclVarRef,tclString,tcltlWidgetSwitch,tcltkCommand,tcltkPackConf

" PACK
" commands associated with pack
syntax keyword tcltkPackSwitch	contained	forget info propogate slaves
syntax keyword tcltkPackConfSwitch	contained	after anchor before expand fill in ipadx ipady padx pady side
syntax region tcltkCommand matchgroup=tcltkCommandColor start="\<pack\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkPackSwitch,tcltkPackConf,tcltkPackConfSwitch,tclNumber,tclVarRef,tclString,tcltkCommand keepend

" STRING
" commands associated with string
syntax keyword tcltkStringSwitch	contained	compare first index last length match range tolower toupper trim trimleft trimright wordstart wordend
syntax region tcltkCommand matchgroup=tcltkCommandColor start="\<string\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkStringSwitch,tclNumber,tclVarRef,tclString,tcltkCommand

" ARRAY
" commands associated with array
syntax keyword tcltkArraySwitch	contained	anymore donesearch exists get names nextelement size startsearch set
" match from command name to ] or EOL
syntax region tcltkCommand matchgroup=tcltkCommandColor start="\<array\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkArraySwitch,tclNumber,tclVarRef,tclString,tcltkCommand

" LSORT
" switches for lsort
syntax keyword tcltkLsortSwitch	contained	ascii dictionary integer real command increasing decreasing index

" match from command name to ] or EOL
syntax region tcltkCommand matchgroup=tcltkCommandColor start="\<lsort\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkLsortSwitch,tclNumber,tclVarRef,tclString,tcltkCommand

syntax keyword tclTodo contained	TODO

" String and Character contstants
" Highlight special characters (those which have a backslash) differently
syntax match   tclSpecial contained "\\\d\d\d\=\|\\."

" A string needs the skip argument as it may legitimately contain \".
" Match at start of line
syntax region  tclString   start=+^"+ end=+"+ contains=tclSpecial skip=+\\\\\|\\"+

"Match all other legal strings.
syntax region  tclString	start=+[^\\]"+ms=s+1  end=+"+ contains=tclSpecial skip=+\\\\\|\\"+

syntax match  tclLineContinue "\\\s*$"

"integer number, or floating point number without a dot and with "f".
syntax case ignore

syntax match  tclNumber	"\<\d\+\(u\=l\=\|lu\|f\)\>"

"floating point number, with dot, optional exponent
syntax match  tclNumber		"\<\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\=\>"

"floating point number, starting with a dot, optional exponent
syntax match  tclNumber		"\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"

"floating point number, without dot, with exponent
syntax match  tclNumber	"\<\d\+e[-+]\=\d\+[fl]\=\>"

"hex number
syntax match  tclNumber	"0x[0-9a-f]\+\(u\=l\=\|lu\)\>"

"Suggestion
"syntax match  tclOperator "[~\-_+*<>\[\]{}=|#@$%&\\/:&\^\.,!?]"

"syntax match  tclIdentifier	"\<[a-z_][a-z0-9_]*\>"
syntax case match

syntax region tclComment	start="^\s*\#" skip="\\$" end="$" contains=tclTodo
syntax region tclComment	start=/;\s*\#/hs=s+1 skip="\\$" end="$" contains=tclTodo

"proc folding
syntax region myFold start="^\z(\s*\)proc\s\(.*$\)\@=" end="^\z1}" transparent fold extend
syntax syntaxc fromstart
set foldmethod=syntaxtax

"syntax syntaxc ccomment tclComment

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_tcl_syntaxtax_inits")
    if version < 508
        let did_tcl_syntaxtax_inits = 1
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

    HiLink tclOperator          Operator

    delcommand HiLink
endif

let b:current_syntaxtax = "tcldev"

" vim: ts=4 sw=4 et
