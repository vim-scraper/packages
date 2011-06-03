" Vim syntax file
" Language:		shell (sh) Korn shell (ksh) bash (sh)
" Maintainer:		Dr. Charles E. Campbell, Jr. <Charles.E.Campbell.1@gsfc.nasa.gov>
" Previous Maintainer:	Lennart Schultz <Lennart.Schultz@ecmwf.int>
" Last Change:	September 26, 2001
" Version: 27
"
" Using the following VIM variables:
" b:is_kornshell               if defined, enhance with kornshell syntax
" b:is_bash                    if defined, enhance with bash syntax
"   is_kornshell               if neither b:is_kornshell or b:is_bash is
"                                 defined, then if is_kornshell is set
"                                 b:is_kornshell is default
"   is_bash                    if none of the previous three variables are
"                                 defined, then if is_bash is set b:is_bash is default
"
" This file includes many ideas from Éric Brunet (eric.brunet@ens.fr)

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Set 'background' back to the default.  The value can't always be estimated
" and is then guessed.
"hi clear Normal
"set bg&

" Remove all existing highlighting and set the defaults.
hi clear

" Load the syntax highlighting defaults, if it's enabled.
"if exists("syntax_on")
"  syntax reset
"endif

" sh syntax is case sensitive
syn case match

" A bunch of useful sh keywords
syn keyword bshConditional	elif	else	then
syn keyword bshFunction	function

syn keyword bshSpecialVariables contained	PATH CDPATH RANDOM SECONDS
syn keyword bshSpecialVariables contained	PS1 PS2 IFS __VLIM__ __ARGS__
syn keyword bshSpecialVariables contained	HOME PWD OLDPWD
syn keyword bshSpecialVariables contained	BSHTEMP TEMPDIR TMPDIR TEMP
syn keyword bshSpecialVariables contained	TMP BSHAUTOR
"syn keyword Statement	.        :
syn keyword bshStatement	                  alias    array    autor    base     basename break
syn keyword bshStatement	cat      catv     cd       chdir    cmpv     continue conv     copy
syn keyword bshStatement	crc      ctime    cut      dirname  echo     env      eval     exec
syn keyword bshStatement	         export   expr     extern   false    fmode    fork     fprint
syn keyword bshStatement	fsize    fstat    fullname global   goend    goto     grep     ifdef
syn keyword bshStatement	ifset    ifenv    inv      kill              line     link     list
syn keyword bshStatement	local    localset mkdirs   mktemp   move     mtime    nop      print
syn keyword bshStatement	prints            pwd      read     readc    readl    readonly rel
syn keyword bshStatement	remove   return            seek     set      shift    sleep    sortl
syn keyword bshStatement	static   stime    sum      system   systime  tee      test     times
syn keyword bshStatement	tr       trap     true     type     typeset  tz       umask    unalias
syn keyword bshStatement	unexport unset    unsetenv ver      wait     wc       whence
syn keyword bshAdminStatement	sane     exit     prompt   let

syn match bshWord	"\<\h\w*\>"

" This one is needed INSIDE a CommandSub, so that `echo bla` be correct
syn cluster bshEchoList	contains=bshNumber,bshArithmetic,bshCommandSub,bshSinglequote,bshDeref,bshSpecialVar,bshSpecial,bshOperator,bshDoubleQuote,bshPosnParm
syn region bshEcho matchgroup=bshStatement start="\<echo\>"  skip="[\\%]$" matchgroup=bshOperator end="$" matchgroup=NONE end="[<>;&|()]"me=e-1 end="\d[<>]"me=e-2 end="#"me=e-1 contains=@bshEchoList
syn region bshEcho matchgroup=bshStatement start="\<print\>" skip="[\\%]$" matchgroup=bshOperator end="$" matchgroup=NONE end="[<>;&|()]"me=e-1 end="\d[<>]"me=e-2 end="#"me=e-1 contains=@bshEchoList

" This must be after the strings, so that bla \" be correct
syn region bshEmbeddedEcho contained matchgroup=bshStatement start="\<echo\>" start="\<print\>" skip="[\\%]$" matchgroup=bshOperator end="$" matchgroup=NONE end="[<>;&|`)]"me=e-1 end="\d[<>]"me=e-2 end="#"me=e-1 contains=bshNumber,bshSinglequote,bshDeref,bshSpecialVar,bshSpecial,bshOperator,bshDoubleQuote

" Error Codes
" ===========
syn match   bshDoError "\<done\>"
syn match   bshIfError "\<fi\>"
syn match   bshInError "\<in\>"
syn match   bshCaseError ";[;,]"
syn match   bshEsacError "\<esac\>"
syn match   bshCurlyError "}"
syn match   bshParenError ")"
syn match   bshDTestError "]]"
syn match   bshTestError "]"

" Options interceptor
syn match   bshOption  "\s[+-][a-zA-Z0-9]\+\>"ms=s+1
syn match   bshOption  "\s--\S\+"ms=s+1

syn match   bshSource	"^\.\s"
syn match   bshSource	"\s\.\s"
syn region  bshColon	start="^\s*:" end="$" end="#"me=e-1 contains=ALLBUT,@bshColonList

" error clusters:
"================
syn cluster bshErrorList	contains=bshCaseError,bshCurlyError,bshDTestError,bshDerefError,bshDoError,bshEsacError,bshIfError,bshInError,bshParenError,bshTestError
syn cluster bshErrorFuncList	contains=bshDerefError
syn cluster bshErrorLoopList	contains=bshCaseError,bshDTestError,bshDerefError,bshDoError,bshInError,bshParenError,bshTestError
syn cluster bshErrorCaseList	contains=bshCaseError,bshDerefError,bshDTestError,bshDoError,bshInError,bshParenError,bshTestError
syn cluster bshErrorColonList	contains=bshDerefError
syn cluster bshErrorNoneList	contains=bshDerefError

" clusters: contains=@... clusters
"==================================
syn cluster bshCaseEsacList	contains=bshCaseStart,bshCase,bshCaseBar,bshCaseIn,bshComment,bshDeref,bshCommandSub
syn cluster bshDblQuoteList	contains=bshCommandSub,bshDeref,bshSpecial,bshPosnParm
syn cluster bshDerefList	contains=bshDerefOp,bshDerefError,bshDerefOpError,bshExpr
syn cluster bshIdList	contains=bshCommandSub,bshWrapLineOperator,bshIdWhiteSpace,bshDeref,bshSpecial

" clusters: contains=ALLBUT,@... clusters
"=========================================
syn cluster bshCaseList	contains=bshCase,bshCaseStart,bshCaseBar,bshDblBrace,bshDerefOp,bshDerefText,@bshErrorCaseList,bshDerefVar,bshDerefOpError,bshStringSpecial,bshSkipInitWS,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier
syn cluster bshColonList	contains=bshCase,bshCaseStart,bshCaseBar,bshDblBrace,bshDerefOp,bshDerefText,bshFunction,bshTestOpr,@bshErrorColonList,bshDerefVar,bshDerefOpError,bshStringSpecial,bshSkipInitWS,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier
syn cluster bshCommandSubList1	contains=bshCase,bshCaseStart,bshCaseBar,bshDblBrace,bshCommandSub,bshDerefOp,bshDerefText,bshEcho,bshFunction,bshTestOpr,@bshErrorList,bshDerefVar,bshDerefOpError,bshStringSpecial,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier
syn cluster bshCommandSubList2	contains=bshCase,bshCaseStart,bshCaseBar,bshDblBrace,bshDerefOp,bshDerefText,bshEcho,bshFunction,bshTestOpr,@bshErrorList,bshDerefVar,bshDerefOpError,bshStringSpecial,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier
syn cluster bshLoopList	contains=@bshErrorLoopList,bshCase,bshCaseStart,bshInEsac,bshCaseBar,bshDerefOp,bshDerefText,bshDerefVar,bshDerefOpError,bshStringSpecial,bshSkipInitWS,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier
syn cluster bshExprList1	contains=bshCase,bshCaseStart,bshCaseBar,bshDblBrace,bshDerefOp,bshDerefText,bshFunction,bshSetList,@bshErrorNoneList,bshDerefVar,bshDerefOpError,bshStringSpecial,bshSkipInitWS,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier
syn cluster bshExprList2	contains=bshCase,bshCaseStart,bshCaseBar,bshDblBrace,bshDerefOp,bshDerefText,@bshErrorNoneList,bshDerefVar,bshDerefOpError,bshStringSpecial,bshSkipInitWS,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier
syn cluster bshSubShList	contains=bshCase,bshCaseStart,bshCaseBar,bshDblBrace,bshDerefOp,bshDerefText,bshParenError,bshDerefVar,bshDerefOpError,bshStringSpecial,bshSkipInitWS,bshDerefError,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier
syn cluster bshTestList	contains=bshCase,bshCaseStart,bshCaseBar,bshDblBrace,bshDTestError,bshDerefError,bshDerefOp,bshDerefText,bshExpr,bshFunction,bshSetList,bshTestError,bshDerefVar,bshDerefOpError,bshStringSpecial,bshSkipInitWS,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier
syn cluster bshFunctionList	contains=bshCase,bshCaseStart,bshCaseBar,bshDblBrace,@bshErrorFuncList,bshDerefOp,bshDerefText,bshFunction,bshDerefVar,bshDerefOpError,bshStringSpecial,bshSkipInitWS,bshIdWhiteSpace,bshDerefTextError,bshPattern,bshSetIdentifier

" Tests
"======
syn region  bshDblBrace transparent matchgroup=bshOperator start="\[\[\s" skip=+[\\%]$+ end="\s]]" end="^]]" contains=ALLBUT,@bshTestList,bshOperator,bshSource,bshColon
syn region  bshExpr transparent matchgroup=bshOperator start="\[\s" skip=+[\\%]$+ end="\s]" end="^]" contains=ALLBUT,@bshTestList,bshOperator,bshSource,bshColon
syn region  bshExpr transparent matchgroup=bshStatement start="\<test\>" skip=+\\\\\|%%\|[\\%]$+ matchgroup=NONE end="[;&|]"me=e-1 end="$" contains=ALLBUT,@bshExprList1,bshSource,bshColon
syn match   bshTestOpr contained "-\(nt\|ot\|ef\|eq\|ne\|lt\|le\|gt\|ge\|[a-zA-Z]\)\>\s"
syn match   bshTestOpr contained "\([!=]=\=\|&&\|||\)\s"
syn region   bshFilenameExp oneline start="\[[^ ]" end="\]"

" do, if, while, until
syn region bshDo  transparent matchgroup=bshConditional start="\<do\>" matchgroup=bshConditional end="\<done\>" contains=ALLBUT,@bshLoopList
syn region bshIf  transparent matchgroup=bshConditional start="\<if\>" matchgroup=bshConditional end="\<fi\>"   contains=ALLBUT,@bshLoopList
syn region bshFor matchgroup=bshStatement start="\<for\>" start="\<from\>" start="\<by\>" start="\<to\>" end="\<in\>" end="\<repeat\>" end="\<do\>"me=e-2	contains=ALLBUT,@bshLoopList,bshWord
syn region bshRepeat   matchgroup=bshStatement   start="\<while\>" end="\<do\>"me=e-2	contains=ALLBUT,@bshLoopList,bshWord
syn region bshRepeat   matchgroup=bshStatement   start="\<until\>" end="\<do\>"me=e-2	contains=ALLBUT,@bshLoopList,bshWord
"syn region bshCaseEsac matchgroup=bshConditional start="\<select\>" matchgroup=bshConditional end="\<in\>" end="\<do\>" contains=ALLBUT,@bshLoopList

" case
syn region  bshCaseEsac	matchgroup=bshConditional start="\<case\>" matchgroup=bshConditional end="\<esac\>" contains=@bshCaseEsacList
syn keyword bshCaseIn	contained skipwhite skipnl in		nextgroup=bshCase,bshCaseStart,bshCaseBar,bshComment
syn region  bshCase	contained skipwhite skipnl matchgroup=bshOperator start="[^$()]\{-})"ms=s,hs=e  matchgroup=bshOperator end=";[;,]" end="esac"me=s-1 contains=ALLBUT,@bshCaseList nextgroup=bshCase,bshCaseStart,bshCaseBar,bshComment
syn match   bshCaseStart	contained skipwhite skipnl "("		nextgroup=bshCase
syn match   bshCaseBar	contained "[^|)]\{-}|"hs=e			nextgroup=bshCase,bshCaseStart,bshCaseBar

syn region bshExpr  transparent matchgroup=bshExprRegion start="{{\s*$" start="{{\s\+" end="^\s*}}" end="\s}}"		contains=ALLBUT,@bshExprList2
syn region bshExpr  transparent matchgroup=bshExprRegion start="{\s*$" start="{\s\+" end="^\s*}" end="\s}"		contains=ALLBUT,@bshExprList2
syn region bshSubSh transparent matchgroup=bshSubShRegion start="(" end=")"	contains=ALLBUT,@bshSubShList

" Misc
"=====
"syn match   bshOperator"[&;|]"
syn match   bshOperator	";\||[|!0-9]\=\|&[&|]\="
"syn match   bshOperator"\[\[[^:]\|]]"
"syn match   bshCharClass"\[:\(backspace\|escape\|return\|xdigit\|alnum\|alpha\|blank\|cntrl\|digit\|graph\|lower\|print\|punct\|space\|upper\|tab\):\]"
syn match   bshOperator	"\s=\=::\=\s"
"syn match   bshOperator"!\=="	skipwhite nextgroup=bshPattern
syn match   bshPattern	"\<\S\+"	contained contains=bshSinglequote,bshDoublequote,bshDeref,bshOperator,bshRedir
syn match   bshWrapLineOperator "[\\%]$"
syn region  bshCommandSub   matchgroup=bshCmdSubRegion start="`-\=" skip="[\\%]`" end="`" contains=ALLBUT,@bshCommandSubList1

" $(..) is not supported by sh (Bourne shell).  However, apparently
" some systems (HP?) have as their /bin/sh a (link to) Korn shell
" (ie. Posix compliant shell).  /bin/ksh should work for those
" systems too, however, so the following syntax will flag $(..) as
" an Error under /bin/sh.  By consensus of vimdev'ers!
syn region bshCommandSub matchgroup=bshCmdSubRegion start="\$(-\=" end=")"  contains=ALLBUT,@bshCommandSubList2
"syn region bshArithmetic matchgroup=bshArithRegion start="\$((" end="))" contains=ALLBUT,@bshCommandSubList2
syn region bshArithKlamm                           contained start="(" end=")"
syn region bshArithmetic matchgroup=bshArithRegion start="\$((" end="))" contains=bshArithKlamm
syn region bshArithmetic2 matchgroup=bshArithRegion start="^\s*((" start="[^$](("hs=s+1 end="))" contains=bshArithKlamm
syn match  bshSkipInitWS contained	"^\s\+"
"syn region bshCommandSub matchgroup=Error start="$(" end=")" contains=ALLBUT,@bshCommandSubList2



" Comments
"=========
syn keyword	bshTodo	contained	TODO
syn cluster	bshCommentGroup	contains=bshTodo
syn match	bshComment		"#.*$" contains=@bshCommentGroup

" String and Character constants
"===============================
syn match   bshNumber		"[+-]\=\<\d\+\>"
syn match   bshSpecial	contained	"[\\%]\d\d\d\|[\\%][abcefjnrstv0]"
syn match   bshSpecialS	contained	"''"
syn region  bshSinglequote	matchgroup=bshOperator start=+'+  skip=+''+ end=+'+	contains=bshStringSpecial,bshSpecialS
syn region  bshDoubleQuote     matchgroup=bshOperator start=+"+ skip=+[\\%]"+ end=+"+	contains=@bshDblQuoteList,bshStringSpecial,bshArithmetic
syn match   bshStringSpecial	contained	"[^[:print:]]"
syn match   bshSpecial		/\\[\\"'`$]\|%[%"'`$]/
"syn match   bshSpecial	contained"%[%\"\'`$]"

" File redirection highlighted as operators
"==========================================
syn match	bshRedir	"[tbBs]\=\d\=>>\=-\="
syn match	bshRedir	"[tbBs]\=\d\=<\(&[-0-9]\)\="
"syn match	bshRedir"[tbBs]\=\d<<\=-\="
syn match	bshRedir	"[tbBs]\=\d<>-\="
syn match	bshRedir	"[^<>]><\+$"ms=s+1
syn match	bshRedir	"[^<>]><\+[^<>]"ms=s+1,me=e-1
syn match	bshRedir	"^><\+[^<>]"me=e-1
syn match	bshRedir	"^><\+$"
"syn match	bshRedir"\d\=>\(&[-0-9]\)\="
"syn match	bshRedir"\d\=>>-\="
"syn match	bshRedir"\d\=<\(&[-0-9]\)\="
"syn match	bshRedir"\d<<-\="

" Shell Input Redirection (Here Documents)
if version < 600
 syn region bshHereDoc matchgroup=bshRedir start="<<\s*\**END[a-zA-Z_0-9]*\**"  matchgroup=bshRedir end="^END[a-zA-Z_0-9]*$"
 syn region bshHereDoc matchgroup=bshRedir start="<<-\s*\**END[a-zA-Z_0-9]*\**" matchgroup=bshRedir end="^\s*END[a-zA-Z_0-9]*$"
 syn region bshHereDoc matchgroup=bshRedir start="<<\s*\**EOF\**"  matchgroup=bshRedir end="^EOF$"
 syn region bshHereDoc matchgroup=bshRedir start="<<-\s*\**EOF\**" matchgroup=bshRedir end="^\s*EOF$"
 syn region bshHereDoc matchgroup=bshRedir start="<<\s*\**\.\**"  matchgroup=bshRedir end="^\.$"
 syn region bshHereDoc matchgroup=bshRedir start="<<-\s*\**\.\**" matchgroup=bshRedir end="^\s*\.$"
else
 syn region bshHereDoc matchgroup=bshRedir start="<<\s*\**\z(\h\w*\)\**"  matchgroup=bshRedir end="^\z1$"
 syn region bshHereDoc matchgroup=bshRedir start="<<-\s*\**\z(\h\w*\)\**" matchgroup=bshRedir end="^\s*\z1$"
endif

" Identifiers
"============
syn match  bshVariable "\<\h\w*="me=e-1	nextgroup=bshSetIdentifier
syn match  bshVariable "\s\$\h\w*="me=e-1	nextgroup=bshSetIdentifier
syn match  bshVariable "^\$\h\w*="me=e-1	nextgroup=bshSetIdentifier
syn match  bshVariable "\s\${\+\h\w*}\+="me=e-1	nextgroup=bshSetIdentifier
syn match  bshVariable "^\${\+\h\w*}\+="me=e-1	nextgroup=bshSetIdentifier
syn match  bshVariable "\s\$\d="me=e-1	nextgroup=bshSetIdentifier
syn match  bshVariable "^\$\d="me=e-1	nextgroup=bshSetIdentifier
syn match  bshVariable "\s\${\+\d\+}\+="me=e-1	nextgroup=bshSetIdentifier
syn match  bshVariable "^\${\+\d\+}\+="me=e-1	nextgroup=bshSetIdentifier
"syn region  bshCatvA start="\<catv\>\s"hs=s+5 end="\s="he=e-2 end="$" contains=bshDeref,bshDerefVar,bshCatvName
"syn match  bshCatvName contained "\<\h\w*\>"
syn match  bshCatvVariable "[=,]\h\w*$"ms=s+1
syn match  bshCatvVariable "[=,]\h\w*[.:;]"ms=s+1,me=e-1
syn match  bshSet_Variable "\s\h\w*:\S"ms=s+1,me=e-2
syn match  bshVariable "\s=\=:\$\=\<\h\w*\>"
syn match  bshVariable "\s=\=:\$\=\<\d\+\>"
syn match  bshIdWhiteSpace  contained	"\s"
syn match  bshSetIdentifier contained	"=" nextgroup=bshStringSpecial,bshPattern
"syn region bshSetList matchgroup=bshStatement start="\<\(typeset\|set\|export\|unset\|local\|static\|ifdef\|ifset\|ifenv\|unexport\)\>[^/]"me=e-1 end="$" end="[)|]"me=e-1 matchgroup=bshOperator end="[;&]"me=e-1 matchgroup=NONE end="[#=]"me=e-1 contains=@bshIdList
"syn match  bshStatement"\<\(typeset\|set\|export\|unset\|local\|static\|ifdef\|ifset\|ifenv\|unexport\)$"

" handles functions which start:  Function () {
"syn match  bshFunction"^\<\h\w*\>()\s\+"	skipwhite skipnl nextgroup=bshFunctionRegion
"syn region bshFunctionRegion	transparent	matchgroup=Delimiter	start="{" end="^}$" contains=ALLBUT,@bshFunctionList
syn region bshFunction	transparent	matchgroup=bshFunctionName start="^\<\h\w*\>()\s\+{" end="^}$" contains=ALLBUT,@bshFunctionList
syn region bshFunction	oneline transparent	matchgroup=bshFunctionName start="^\<\h\w*\>()\s\+{\s" end="\s}$" contains=ALLBUT,@bshFunctionList
syn region bshFunction1	transparent	matchgroup=bshFunctionName1	start="^\s\+\<\h\w*\>())\s\+{$" end="^\s\+)}$" contains=ALLBUT,@bshFunctionList,bshFunction1
syn region bshFunction1	oneline transparent	matchgroup=bshFunctionName1	start="^\s\+\<\h\w*\>())\s\+{\s" end="\s)}$" contains=ALLBUT,@bshFunctionList,bshFunction1
syn region bshFunction2	transparent	matchgroup=bshFunctionName2	start="^\s\+\<\h\w*\>()))\s\+{$" end="^\s\+))}$" contains=ALLBUT,@bshFunctionList,bshFunction2
syn region bshFunction2	oneline transparent	matchgroup=bshFunctionName2	start="^\s\+\<\h\w*\>()))\s\+{\s" end="\s))}$" contains=ALLBUT,@bshFunctionList,bshFunction2

syn region bshDeref	oneline	start="\${\+" end="}\+"	contains=bshDeref,bshDerefVar
syn match  bshDeref		"\$\h\w*\>\|\$\d\+"
syn match  bshDerefVar	contained	"\d\+\>"		nextgroup=@bshDerefList
syn match  bshDerefVar	contained	"\h\w*\>"		nextgroup=@bshDerefList
syn match  bshDerefVar	contained	"[-#@*?!]"		nextgroup=@bshDerefList
syn match  bshDerefOp	contained	":[^-=?+]"me=e-1	nextgroup=bshDeref,bshDerefVar,bshPosnParm
syn match  bshDerefVar	contained	"\$[^{(]"me=s+1	nextgroup=@bshDerefList
syn match  bshPosnParm		"\$[]}.:#@*$?!0-9-]"
"syn match  bshPosnParm"\$[]}.:#@*$?!0-9-]"	nextgroup=bshDerefOp
syn match  bshDerefOpError	contained	"[^:}[]"
syn match  bshDerefOp	contained	":\=[-=?+]"		nextgroup=bshDerefText
syn region bshDerefText	contained	start="[^{}]"	end="}"me=e-1	contains=bshDeref,bshCommandSub,bshDoubleQuote,bshSingleQuote,bshDerefTextError
syn match  bshDerefTextError	contained	"^."
syn match  bshDerefError	contained	"\s.\{-}}"me=e-1
syn region bshDerefText	contained	start="{"	end="}"	contains=bshDeref,bshCommandSub

syn match bshDerefVar	contained"#\(\d\+\|\h\w*\)"	nextgroup=bshDerefOp,bshDerefError,bshDerefOpError,bshExpr
"syn match bshDerefOp	contained"##\|%%\|[#%]"		nextgroup=bshDerefText


" Syncs
" =====
if !exists("sh_minlines")
  let sh_minlines = 200
endif
if !exists("sh_maxlines")
  let sh_maxlines = 2 * sh_minlines
endif
exec "syn sync minlines=" . sh_minlines . " maxlines=" . sh_maxlines
syn sync match bshCaseEsacSync grouphere	bshCaseEsac	"\<case\>"
syn sync match bshCaseEsacSync groupthere	bshCaseEsac	"\<esac\>"
syn sync match bshDoSync	grouphere	bshDo	"\<do\>"
syn sync match bshDoSync	groupthere	bshDo	"\<done\>"
syn sync match bshForSync	grouphere	bshFor	"\<for\>"
syn sync match bshForSync	groupthere	bshFor	"\<in\>"
syn sync match bshIfSync	grouphere	bshIf	"\<if\>"
syn sync match bshIfSync	groupthere	bshIf	"\<fi\>"

syn sync match bshUntilSync	grouphere	bshRepeat	"\<until\>"
syn sync match bshWhileSync	grouphere	bshRepeat	"\<while\>"

" The default highlighting.
hi  link bshArithRegion		bshArith
hi  link bshCaseBar		bshConditional
hi  link bshCaseIn		bshConditional
hi  link bshCaseStart		bshConditional
hi  link bshCmdSubRegion		CmdSubst
hi  link bshColon		bshStatement
hi  link bshDeref		bshShellVariables
hi  link bshDerefOp		bshOperator
hi  link bshDerefVar		bshShellVariables
hi  link bshDoubleQuote		bshString
hi  link bshEcho		bshString
hi  link bshEmbeddedEcho		bshString
hi  link bshHereDoc		bshString
"hi  link bshOption		bshCommandSub
hi  link bshPattern		bshString
hi  link bshPosnParm		bshShellVariables
hi  link bshRedir		bshOperator
hi  link bshSinglequote		bshString
hi  link bshSource		bshOperator
hi  link bshStringSpecial		bshSpecialS
hi  link bshSubShRegion		bshOperator
hi  link bshTestOpr		bshOperator
hi  link bshWrapLineOperator		bshOperator
hi  link bshAdminStatement		bshArith
hi  link bshSpecialVariables		StdName

hi  link bshCaseError		Error
hi  link bshCurlyError		Error
hi  link bshDerefError		Error
hi  link bshDerefOpError		Error
hi  link bshDerefTextError		Error
hi  link bshDoError		Error
hi  link bshEsacError		Error
hi  link bshIfError		Error
hi  link bshInError		Error
hi  link bshParenError		Error
hi  link bshTestError		Error
hi  link bshDTestError		Error
"hi  link bshCommandSub		Normal
hi  link bshComment		Comment
hi  link bshConditional		Statement
hi  link bshExprRegion		Statement
"hi  link bshExprRegion		Delimiter
hi  link bshFunction		Function
hi  link bshFunctionName		Function
hi  link bshFunction1		SubFunction
hi  link bshFunctionName1		SubFunction
hi  link bshFunction2		SubFunction
hi  link bshFunctionName2		SubFunction
hi  link bshWord		Word
hi  link bshNumber		Number
hi  link bshOperator		Operator
hi  link bshRepeat		LoopList
hi  link bshFor		LoopList
hi  link bshSetList		Identifier
hi  link bshVariable		Identifier
hi  link bshCatvVariable		Identifier
hi  link bshSet_Variable		Identifier
hi  link bshCatvName		PreProc
hi  link bshShellVariables		PreProc
hi  link bshSpecial		Special
hi  link bshSpecialS		SpecialS
hi  link bshStatement		Statement
hi  link bshString		Constant
hi  link bshType		Type
hi  link bshArith		Arith
hi  link bshArithmetic		ArithCont
hi  link bshArithmetic2		ArithCont
hi  link bshArithKlamm		ArithCont
hi  link bshTodo		Todo

hi Comment		gui=NONE	guifg=grey74 	guibg=NONE
hi Constant		gui=NONE 	guifg=Magenta 	guibg=NONE
hi Special		gui=NONE 	guifg=#b000b0 guibg=NONE
hi Identifier		gui=NONE 	guifg=DarkCyan guibg=NONE
hi Statement		gui=bold 	guifg=#a06129 	guibg=NONE
hi PreProc		gui=NONE 	guifg=Purple 	guibg=NONE
hi Type		gui=bold 	guifg=SeaGreen guibg=NONE
hi Underlined		gui=underline 	guifg=SlateBlue
hi Ignore		gui=NONE 	guifg=bg 	guibg=NONE
hi Error		gui=NONE 	guifg=White 	guibg=Red
hi Todo		gui=NONE 	guifg=Blue 	guibg=Yellow
hi Normal		gui=NONE 	guifg=#000000 	guibg=Ivory1
hi SpecialS		gui=NONE guifg=#000000 guibg=NONE
hi LoopList		gui=NONE guifg=SeaGreen
hi Function		gui=bold guifg=#e86f00
hi SubFunction		gui=NONE guifg=#e86f00
hi Operator		gui=bold guifg=#000000
hi CmdName		gui=NONE guifg=#a06129
hi StdName		gui=bold guifg=#5276e6
hi Name		gui=NONE guifg=#5276e6
hi Word		gui=NONE guifg=#a06129
hi CmdSubst		gui=bold guifg=#0000ff guibg=#eeeeff
hi CmdSubstCont	gui=NONE guifg=#0000ff guibg=#ccccff
hi Arith		gui=bold guifg=#00aa00
hi ArithCont		gui=NONE guifg=#008800
"hi BackGnd		gui=bold guifg=red guibg=grey98

" Current Syntax
" ==============
let b:current_syntax = "bsh"
set nocindent

" vim: ts=15
