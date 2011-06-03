" Vim syntax file
" Language:	    Tcl/Tk
" Maintainer:   Robert Hicks <sigzero@gmail.com>
" Last Change:  2006 Jul 11
" Version:      0.9
"
" Prev Maintainers:
"   Dean Copsey <copsey@cs.ucdavis.edu>
"	Matt Neumann <mattneu@purpleturtle.com>
"	Allan Kelly <allan@fruitloaf.co.uk>
"   Robin Becker <robin@jessikat.demon.co.uk>
"
" Helped in some way:
"   Tomasz Kalkosinski
"   Mark Smithfield
"

" ------------------------------
"  CLEAR CURRENT SYNTAX
" ------------------------------
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif


" --------------------------------
"  OPTIONS <these go in your vimrc
" --------------------------------
"  let tcl_extended_syntax=0
"  let tcl_extended_syntax=1


" ------------------------------
"  SYNTAX: Tcl Keywords
" ------------------------------
syn match tclExternal   "\<\(package\)\s\(forget\|ifneeded\|names\|present\|provide\|require\|unknown\|vcompare\|versions\|satisfies\)\>"

syn keyword tclLabel		case default
syn keyword tclConditional  if then else elseif switch
syn keyword tclLooping		while for foreach break continue
syn keyword tclExceptions   catch error break return

syn keyword tclKeyword    after append auto_execok auto_import auto_load
syn keyword tclKeyword    auto_mkindex auto_mkindex_old auto_qualify auto_reset
syn keyword tclKeyword    bgerror binary cd chan clock close concat dde dict encoding
syn keyword tclKeyword    eof eval exec exit fblocked fconfigure fcopy file
syn keyword tclKeyword    fileevent flush gets glob global history http
syn keyword tclKeyword    incr info interp join lappend lindex linsert list
syn keyword tclKeyword    llength load lrange lreplace lsearch lset memory
syn keyword tclKeyword    msgcat open parray pid pkg::create pkg_mkIndex
syn keyword tclKeyword    proc puts pwd read regexp registry regsub rename resource
syn keyword tclKeyword    scan seek set socket source split subst tcl_endOfWord
syn keyword tclKeyword    tcl_findLibrary tcl_startOfNextWord tcl_startOfPreviousWord
syn keyword tclKeyword    tcl_wordBreakAfter tcl_wordBreakBefore tell time trace
syn keyword tclKeyword    unknown unset update uplevel upvar vwait

syn keyword tkKeyword   bell bind bindtags canvas checkbutton clipboard
syn keyword tkKeyword   console destroy entry event focus font frame grab grid
syn keyword tkKeyword   image label labelframe listbox lower menu menubutton
syn keyword tkKeyword   message option panedwindow place radiobutton raise
syn keyword tkKeyword   scrollbar selection send spinbox text tk tk_bisque
syn keyword tkKeyword   tk_chooseColor tk_chooseDirectory tk_dialog tk_focusFollowsMouse
syn keyword tkKeyword   tk_focusNext tk_focusPrev tk_getOpenFile tk_getSaveFile
syn keyword tkKeyword   tk_menuSetFocus tk_messageBox tk_optionMenu tk_popup tk_setPalette
syn keyword tkKeyword   tk_textCopy tk_textCut tk_textPaste tkerror tkwait toplevel
syn keyword tkKeyword   winfo wm

syn keyword tcltkSwitch	contained   insert create polygon fill outline tag

" ------------------------------
"  SYNTAX: Tcl Namespace Stuff
" ------------------------------
syn match tclNamespace "$\(\(::\)\?\([[:alnum:]_]*::\)*\)\a[a-zA-Z0-9_]*"
" ${...} may contain any character except '}'
syn match tclNamespace "${[^}]*}"

if tcl_extended_syntax
    " This is an attempt to capture as much of the keywords from itcl, Snit, XOTcl
    " and the new 8.5 oo:: object frameworks
    "
    " TODO: rework after 8.5 comes out and oo:: is finalized
    syntax keyword ooKeyword  Class class Object body code common component constructor
    syntax keyword ooKeyword  delegate destructor expose filters inherit instances instinvar
    syntax keyword ooKeyword  instproc isa itcl_class itcl_info local macro metaclass method
    syntax keyword ooKeyword  mixins oncget onconfigure parameters part pragma private
    syntax keyword ooKeyword  protected public scope self subclasses super superclasses
    syntax keyword ooKeyword  type typecomponent typemethod typeof typevariable usual widget

    " These will match the following namespaces: ttk::, snit::, itcl::, oo::
    syntax match tclNamespace "\(ttk\)\(\(::\)\?\([[:alnum:]_.]*::\)*\)\a[a-zA-Z0-9_.]*"
    syntax match tclNamespace "\(^snit\)\(\(::\)\?\([[:alnum:]_.]*::\)*\)\a[a-zA-Z0-9_.]*"
    syntax match tclNamespace "\(^itcl\)\(\(::\)\?\([[:alnum:]_.]*::\)*\)\a[a-zA-Z0-9_.]*"
    syntax match tclNamespace "\(^oo\)\(\(::\)\?\([[:alnum:]_.]*::\)*\)\a[a-zA-Z0-9_.]*"
endif


" ------------------------------
"  OPTIONS: matches like -text, -padx, -fill
" ------------------------------
syn match tclOptionMatcher "\%(^\|\s\)\zs-[a-za-z]\+" contains=tclOptionStarter
syn match tclOptionStarter contained "-"

" match commands - 2 lines for pretty match.
"variable
" Special case - If a number follows a variable region, it must be at the end of
" the pattern, by definition. Therefore, (1) either include a number as the region
" end and exclude tclNumber from the contains list, or (2) make variable
" keepend. As (1) would put variable out of step with everything else, use (2).
syn region tcltkCommand matchgroup=tcltkCommandColor start="^\<variable\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tclString,tclNumber,tclNamespace,tcltkCommand
syn region tcltkCommand matchgroup=tcltkCommandColor start="\s\<variable\>\|\[\<variable\>"hs=s+1 matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tclString,tclNumber,tclNamespace,tcltkCommand

" This isn't contained (I don't think) so it's OK to just associate with the Color group.
" TODO: This could be wrong.
syn keyword tcltkWidgetColor    toplevel

syn region tcltkPackConf matchgroup=tcltkPackConfColor start="\<configure\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkWidgetSwitch,tclString,tcltkSwitch,tcltkPackConfSwitch,tclNumber,tclNamespace keepend
syn region tcltkPackConf matchgroup=tcltkPackConfColor start="\<cget\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkWidgetSwitch,tclString,tcltkSwitch,tcltkPackConfSwitch,tclNumber,tclNamespace

" NAMESPACE
syn keyword tcltkNamespaceSwitch contained children code current delete eval
syn keyword tcltkNamespaceSwitch contained export forget import inscope origin
syn keyword tcltkNamespaceSwitch contained parent qualifiers tail which command variable
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<namespace\>" matchgroup=NONE skip="^\s*$" end="{\|}\|]\|\"\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkNamespaceSwitch

" EXPR
syn keyword tcltkMaths	contained	acos cos hypot sinh
syn keyword tcltkMaths	contained	asin cosh log sqrt
syn keyword tcltkMaths	contained	atan exp log10 tan
syn keyword tcltkMaths	contained	atan2 floor	pow	tanh
syn keyword tcltkMaths	contained	ceil fmod sin
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<expr\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkMaths,tclNumber,tclNamespace,tclString,tcltlWidgetSwitch,tcltkCommand,tcltkPackConf

" WIDGETS
syn keyword tcltkWidgetSwitch contained background highlightbackground insertontime
syn keyword tcltkWidgetSwitch contained selectborderwidth borderwidth highlightcolor insertwidth
syn keyword tcltkWidgetSwitch contained selectforeground cursor highlightthickness padx setgrid
syn keyword tcltkWidgetSwitch contained exportselection insertbackground pady takefocus
syn keyword tcltkWidgetSwitch contained font insertborderwidth relief xscrollcommand
syn keyword tcltkWidgetSwitch contained foreground insertofftime selectbackground yscrollcommand
syn keyword tcltkWidgetSwitch contained height spacing1 spacing2 spacing3
syn keyword tcltkWidgetSwitch contained state tabs width wrap

" BUTTON
syn keyword tcltkWidgetSwitch contained command default

" CANVAS
syn keyword tcltkWidgetSwitch contained closeenough confine scrollregion xscrollincrement yscrollincrement orient

" CHECKBUTTON, RADIOBUTTON
syn keyword tcltkWidgetSwitch contained indicatoron offvalue onvalue selectcolor selectimage state variable

" ENTRY, FRAME
syn keyword tcltkWidgetSwitch contained show colormap container visual

" LISTBOX, MENU
syn keyword tcltkWidgetSwitch contained selectmode postcommand selectcolor tearoff tearoffcommand title type

" MENUBUTTON, MESSAGE
syn keyword tcltkWidgetSwitch contained direction aspect justify

" SCALE
syn keyword tcltkWidgetSwitch contained bigincrement digits from length resolution showvalue sliderlength sliderrelief tickinterval to

" SCROLLBAR
syn keyword tcltkWidgetSwitch contained activerelief elementborderwidth

" IMAGE
syn keyword tcltkWidgetSwitch contained delete names types create

" FORMAT
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<format\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"me=e-1  contains=tclLineContinue,tcltkMaths,tclNumber,tclNamespace,tclString,tcltlWidgetSwitch,tcltkCommand,tcltkPackConf

" MENU, MANE ADD
syn keyword tcltkWidgetSwitch contained active end last none cascade checkbutton command radiobutton separator
syn keyword tcltkWidgetSwitch contained activebackground actveforeground accelerator background bitmap columnbreak
syn keyword tcltkWidgetSwitch contained font foreground hidemargin image indicatoron label menu offvalue onvalue
syn keyword tcltkWidgetSwitch contained selectcolor selectimage state underline value variable
syn keyword tcltkWidgetSwitch contained add clone configure delete entrycget entryconfigure index insert invoke
syn keyword tcltkWidgetSwitch contained post postcascade type unpost yposition activate

syn region tcltkWidget matchgroup=tcltkWidgetColor start="\<button\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1 contains=tclLineContinue,tcltkWidgetSwitch,tclString,tcltkSwitch,tclNumber,tclNamespace
syn region tcltkWidget matchgroup=tcltkWidgetColor start="\<scale\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkWidgetSwitch,tclString,tcltkSwitch,tclNumber,tclNamespace

" PACK
syn keyword tcltkPackSwitch	contained	forget info propagate slaves
syn keyword tcltkPackConfSwitch	contained	after anchor before expand fill in ipadx ipady padx pady side
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<pack\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkPackSwitch,tcltkPackConf,tcltkPackConfSwitch,tclNumber,tclNamespace,tclString,tcltkCommand keepend

" STRING
syn keyword tcltkStringSwitch	contained	compare first index last length match range tolower toupper trim trimleft trimright wordstart wordend
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<string\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkStringSwitch,tclNumber,tclNamespace,tclString,tcltkCommand

" ARRAY
syn keyword tcltkArraySwitch	contained	anymore donesearch exists get names nextelement size startsearch set

" match from command name to ] or EOL
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<array\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkArraySwitch,tclNumber,tclNamespace,tclString,tcltkCommand

" LSORT
syn keyword tcltkLsortSwitch contained  ascii dictionary integer real command increasing decreasing index

" match from command name to ] or EOL
syn region tcltkCommand matchgroup=tcltkCommandColor start="\<lsort\>" matchgroup=NONE skip="^\s*$" end="]\|[^\\]*\s*$"he=e-1  contains=tclLineContinue,tcltkLsortSwitch,tclNumber,tclNamespace,tclString,tcltkCommand

syn keyword tclTodo contained	TODO

" String and Character contstants
" Highlight special characters (those which have a backslash) differently
syn match tclSpecial contained "\\\d\d\d\=\|\\."

" A string needs the skip argument as it may legitimately contain \".
" Match at start of line
syn region tclString    start=+^"+ end=+"+ contains=tclSpecial skip=+\\\\\|\\"+

" Match all other legal strings.
syn region tclString    start=+[^\\]"+ms=s+1  end=+"+ contains=tclSpecial skip=+\\\\\|\\"+

syn match tclLineContinue "\\\s*$"

" integer number, or floating point number without a dot and with "f".
syn case ignore

syn match tclNumber	"\<\d\+\(u\=l\=\|lu\|f\)\>"

" floating point number, with dot, optional exponent
syn match tclNumber		"\<\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\=\>"

" floating point number, starting with a dot, optional exponent
syn match tclNumber		"\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"

" floating point number, without dot, with exponent
syn match tclNumber	"\<\d\+e[-+]\=\d\+[fl]\=\>"

" hex number
syn match tclNumber	"0x[0-9a-f]\+\(u\=l\=\|lu\)\>"

" TODO clashes with the "-" option section
"syntax match tclOperator "[~\-_+*<>\[\]{}=|#@$%&\\/:&\^\.,!?]"

"syntax match tclIdentifier	"\<[a-z_][a-z0-9_]*\>"
syn case match

syn region tclComment   start="^\s*\#" skip="\\$" end="$" contains=tclTodo
syn region tclComment	start=/;\s*\#/hs=s+1 skip="\\$" end="$" contains=tclTodo


" ------------------------------
"  PROC: Folding
" ------------------------------
syn region myFold start="^\z(\s*\)proc\s\(.*$\)\@=" end="^\z1}" transparent fold extend
set foldmethod=syntax


" ------------------------------
"  DEFAULT HIGHLIGHTING
" ------------------------------
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
    HiLink tclKeyword		    Statement
    HiLink tkKeyword            Statement
    HiLink ooKeyword            Statement
    "HiLink tclKeywordColor	Statement
    HiLink tclString		    String
    HiLink tclComment		    Comment
    HiLink tclSpecial		    Special
    HiLink tclTodo		        Todo
    HiLink tcltkCommandColor    Statement
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
    HiLink tcltkPackConfColor   Identifier
    "HiLink tcltkLsort		    Statement
    HiLink tclNamespace		    Identifier
    HiLink tclOptionMatcher     Underlined
    HiLink tclExceptions        Exception
    HiLink tclExternal          Include
    HiLink tclOperator          Operator
    HiLink tclNamespace         Identifier

    delcommand HiLink
endif


" ------------------------------
"  SET SYNC and FILETYPE
" ------------------------------
syn sync fromstart
let b:current_syntax = "tcl"


" vim: ts=4 sw=4 et
