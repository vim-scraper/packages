"syntax clear
"syntax case ignore
"syn keyword qqcKeywords		ask resp for next
"syn region qqcString		start="'" end="'"
"hi link qqcString	String
"hi link qqcKeywords	Keyword
" Vim syntax file
" Language:	Quancept
" Maintainer:	Ferenc Tamasi <ferenc.tamasi@hu.millwardbrown.com>
" Last Change:	Mon Jun 30 18:06 CET 2003

" Based on the BASIC syntax file of Allan Kelly

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore
syn keyword qqcKeywords		allow arrays ask atoz audio
syn keyword qqcKeywords		bkcolor bkground bkstyle callfunc cardcols
syn keyword qqcKeywords		autojump axis bkcolor bkground bkstyle callfunc
syn keyword qqcKeywords		cardcols coded codscheme col colgrid column
"syn keyword qqcKeywords		column comment connect continue control datamap
syn keyword qqcKeywords		column connect continue control datamap
syn keyword qqcKeywords		dbase dbquery decrm define dfmt disallow
syn keyword qqcKeywords		display dk dummyask dummyother echo editopen
syn keyword qqcKeywords		else empty end eximport export file fix for
syn keyword qqcKeywords		force formhead formquest freeze go goto gotosms
syn keyword qqcKeywords		grouped highlight hsetcols htmlfmt if import
syn keyword qqcKeywords		impupdate in include layout list main mapothzero
syn keyword qqcKeywords		maxrecord maxtime maxvideo mcodscheme messagebox
syn keyword qqcKeywords		mintime mmaudio mmcaption mmpicsize mmpicture
syn keyword qqcKeywords		mmpicunit mmvideo mp multiask multidbase newcard
syn keyword qqcKeywords		newline newpage newran newrot next noblank
syn keyword qqcKeywords		noclean nodata not notemp null num ofmt onresp
syn keyword qqcKeywords		other othzero parsesmall pass pause pgstyle
syn keyword qqcKeywords		print privsig promptoth protect qexit qfmt qname
syn keyword qqcKeywords		quota ran ranend ranstart real ref remove resp
syn keyword qqcKeywords		rfclose rfmt rfopen rfprint rlayout rot rotend
syn keyword qqcKeywords		rotranfix rotransub rotstart route rowgrid
syn keyword qqcKeywords		scodscheme screener scribble serialcols
syn keyword qqcKeywords		setsetcols setfmt setstr sfmt showform signal
syn keyword qqcKeywords		slider sp start step stop ststest substitute
syn keyword qqcKeywords		substr textarea textline to unfreeze unpoolable
syn keyword qqcKeywords		unprotect unset useform usefile vsetcols
syn keyword qqcKeywords		warntemp windfmt winfmt winpfmt winqfmt winrfmt
syn keyword qqcKeywords		xdisplay xor xprotect 

syn keyword qqcKeywords		limitresp set

syn keyword qqcSpecResp		null ref dk

syn keyword qqcFunctions	bit nbit numv numb callfunc
syn keyword qqcFunctions	and bit dbrecno elm integer logical mention nbit
syn keyword qqcFunctions	nselrec nsubstr numb numv or othertext selfld
syn keyword qqcFunctions	selrec substr xor
syn keyword qqcSpecKeywords	cell date dayofweekn dayofweeks heaptop ifsnap
syn keyword qqcSpecKeywords	intname iteration login partofday partofdayr
syn keyword qqcSpecKeywords	qcproject random respondent rotation subscript
syn keyword qqcSpecKeywords	tcomment telephone teltext testingrun time
syn keyword qqcSpecKeywords	timeofday timeofdayr userno
syn keyword qqcVariables	curpgstyle hideprev hidestop language logsql
syn keyword qqcVariables	msgalrcmp msgansp msgbrbut msgcntac msgcntam
syn keyword qqcVariables	msgcntan msgcntar msgdk msgend msginvan
syn keyword qqcVariables	msgmisac msgmisam msgmisan msgmisar msgnull
syn keyword qqcVariables	msgnumac msgnumam msgnuman msgnumar msgoth
syn keyword qqcVariables	msgothal msgotiam msgotian msgotmam msgotman
syn keyword qqcVariables	msgref msgrngac msgrgto msgrgor msgrngam
syn keyword qqcVariables	msgrngan msgrngar msgsngac msgsngam msgsngan
syn keyword qqcVariables	msgsngar msgsolac msgsolam msgsolan msgsolar
syn keyword qqcVariables	noaneqnl pgtitle propprefix silentnl
syn keyword qqcFunctions2	abendivr aca acctinfo adddatepart alphdate
syn keyword qqcFunctions2	append autostop chgstop commitvr convdata
syn keyword qqcFunctions2	datepart datetext datetime disable_audio_record
syn keyword qqcFunctions2	enable_audio_record getdbvar getenv getsmvar
syn keyword qqcFunctions2	keepstopped limitresp local longmath longtext
syn keyword qqcFunctions2	makeappt prepareivr putdbvar putsmvar qc_hangup
syn keyword qqcFunctions2	querysms quitdata quorat quotacell quotacnt
syn keyword qqcFunctions2	quotamap quotaval quoval readfile realmath
syn keyword qqcFunctions2	redial rfclose rfopen rfprint setcols setdata
syn keyword qqcFunctions2	setdataser setdatepart setinteger setivr setlang
syn keyword qqcFunctions2	setlocale setreal showinf smscript stopdata
syn keyword qqcFunctions2	strngchk subprog substr testmode timesect
syn keyword qqcFunctions2	valstring varlist
syn keyword qqcVariables2	quotrtxt quotrval smscrres smscrtxt smscrval
syn keyword qqcSpecResp		dk null ref

"syn match   qqcUserVar		"[a-zA-Z0-9]\+"
syntax case match
syn match   qqcTODO		"\<TODO\>\|\<FIX\>" contained
syntax case ignore
syn match   qqcQuestionPart2	"\(q\|s\)[0-9]\+"
"syn match   qqcQuestionPart	"\(q\|s\)[0-9]\+\w\{0,1}\>"
syn match   qqcQuestionLabel2	"^\(q\|s\)[0-9]\+\>"
syn match   qqcQuestionLabel	"^\(q\|s\)[0-9]\+\w\{0,1}\>"
syn match   qqcInstrVar		"\[+.*+\]" contained
syn match   qqcSpecialResp	"\^o'\|\^s'"me=e-1 contained
syn match   qqcComment		"comment.*$" contains=qqcTODO
syn match   qqcNewLine		"@"
syn match   qqcUniqID		"('.*')"
syn match   qqcIllegalChar	"[^a-zA-Z0-9@#$%^&*()_.:+\[\]{}',<> \-\?/]" contained
syn region  qqcString		start="'" end="'" skip="''" contains=qqcInstrVar,qqcSpecialResp,qqcNewLine,qqcIllegalChar
syn match   qqcAutoSubscr	"(\*)"


"integer number, or floating point number without a dot.
syn match  qqcNumber		"\<\d\+\>"
"floating point number, with dot
syn match  qqcNumber		"\<\d\+\.\d*\>"
"floating point number, starting with a dot
syn match  qqcNumber		"\.\d\+\>"

"syn region  qqcLabel		start="^\w" end="\s"
syn match   qqcOperator		"-\|=\|[:<>+\*^/\\]\|\.not\.\|\.or\.\|\.and\.\|\.xor\."

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
"if version >= 508 || !exists("did_basic_syntax_inits")
"  if version < 508
"    let did_basic_syntax_inits = 1
"    command -nargs=+ HiLink hi link <args>
"  else
"    command -nargs=+ HiLink hi def link <args>
"  endif

  hi link qqcIllegalChar	Error
"  hi link qqcUserVar		Preproc
  hi link qqcNewLine		Normal
  hi link qqcQuestionPart2	WarningMsg
"  hi link qqcQuestionPart	WarningMsg
  hi link qqcQuestionLabel2	Error
  hi link qqcQuestionLabel	Error
  hi link qqcAutoSubscr		Number
  hi link qqcErrResp		Error
  hi link qqcKeywords		Keyword
  hi link qqcSpecKeywords	Keyword
  hi link qqcSpecResp		Keyword
  hi link qqcFunctions		Function
  hi link qqcFunctions2		Function
  hi link qqcVariables		Identifier
  hi link qqcVariables2		Identifier
  hi link qqcSpecialResp	Normal
  hi link qqcInstrVar		Preproc
  hi link qqcNumber		Number
  hi link qqcError		Error
  hi link qqcStatement		Statement
  hi link qqcString		String
  hi link qqcComment		Comment
  hi link qqcSpecial		Special
  hi link qqcTodo		Todo
  hi link qqcUniqID		Identifier
  hi link qqcOperator		Operator 
"  delcommand HiLink
"endif

let b:current_syntax = "qqc"

" vim: ts=8
