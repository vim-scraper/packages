" Vim syntax file
" Language:	iHTML
" Maintainer:	Mark Feeney <vim|AT|markfeeney|DOT|com>
" Last Change:	$Id: ihtml.vim,v 1.2 2002/03/26 19:26:59 markf Exp $
" URL:		http://www.markfeeney.com/resources/vim/syntax/ihtml.vim
" Filenames:	*.iHTML, *.inc (some), *.iht
"               NOTE: currently this file isn't part of the vim distro, so you
"               have to manually set the file-type detection stuff.  See the
"               manual for how.
"
" INSTALLATION:
"
" win32:
" 	Copy this file to c:\vim\vimfiles\syntax
" 	Load up a new or existing iHTML file in vim
" 	Give the command: :set syn=ihtml
" 
" Unix-ish:
"       Copy this file to ~/.vim/syntax and follow the win32 insturctions
"
" It doesn't work!
" 	Try running the following commands from in vim, or add them to your
" 	vimrc file:
" 	:syn enable
" 	:colorscheme pablo (or any colorscheme)
" 	:set syn=ihtml
"
" OPTIONS:
"	ihtml_no_sql_hi - set this to anything to disabled the highlighting of
"	SQL within SQL= sections.
" CREDITS:
"   The original version of ihtml.vim version was derived by Mark Feeney from
"   Claudio Fleiner's html.vim, Lutz Eymers' php.vim, and also from Johannes
"   Zellner's xml.vim.
"
" REFERENCES:
"   [1] http://www.ihtml.com/   (Official iHTML manual)
"   [2] http://www.ihtml.co.nz/ (Drew's quick-ref guide)
" 
" TODO:
"   - Date format highlighting within FMT= (maybe)
"   - printf formatting in the appropriate spots (maybe)
"   - JavaScript highlighting within <script> ... </script> tags (maybe)
"   - Option to turn on HTML highlighting (maybe)
"

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif

" Check to see if user wants to have SQL highlighting within SQL=
if !exists("ihtml_no_sql_hi")
  syn include @iHTMLAddSql $VIMRUNTIME/syntax/sql.vim
  unlet b:current_syntax
  syn cluster iHTMLAddSql remove=sqlComment,sqlString
endif

let s:iHTML_cpo_save = &cpo
set cpo&vim

" iHTML in not case sensitive
syn case ignore

"
" iHTMLInside is a cluster of groups representing syntax elements that can
" occur within iHTML strings
"
syn cluster iHTMLInside contains=iHTMLEnvVar,iHTMLVarPunct,iHTMLVar,iHTMLSysVar,iHTMLNumber,iHTMLFloat,iHTMLTag,iHTMLEndTag,iHTMLEqual

" strings (inside tags) aka VALUES
"
" EXAMPLE:
"
" <iLINK SRC = "some string here">
"              ^^^^^^^^^^^^^^^^^^
" NOTES:
"   matchgroup is used to highlight the quotes themselves as a different group
"   than the contents of the string
syn region  iHTMLString contained matchgroup=iHTMLQuote start=+"+ end=+"+ contains=@iHTMLInside
syn region  iHTMLString contained matchgroup=iHTMLQuote start=+'+ end=+'+ contains=@iHTMLInside
syn region  iHTMLString contained matchgroup=iHTMLQuote start=+`+ end=+`+ contains=@iHTMLInside

" We'll even give the lowly equal sign a special colour
syn match   iHTMLEqual +=+ contained

" Expressions
"
" EXAMPLE:
" 	<iIF EXPR=":blah NEQ 13">Do stuff</iIF>
"            ^^^^^^^^^^^^^^^^^^^
" FIXME:
"   This is getting a bit weird; the idea is that iHTML math/logic operators
"   are really only valid within EXPR="" style attributes.  The
"   iHTMLExpression and iHTMLAllQuotes groups are a bit of hack, but they get
"   the job done.  There are better ways to do this, but it's usable as is.
" 
syn region iHTMLExpression 
	\ contained 
	\ transparent
	\ keepend
	\ start=+\sEXPR\s*=\s*\z(["'`]\)+
	\ end=+\z1+
	\ contains=iHTMLOperator,iHTMLEqual,iHTMLVar,iHTMLEnvVar,iHTMLSysVar,iHTMLAllQuotes,iHTMLTag,iHTMLExpressionAttrib
syn keyword iHTMLExpressionAttrib EXPR

" SQL
"
" EXAMPLE:
" 	<iSQLMORE ALIAS="sql1" SQL="SELECT * FROM tblOne">
"                              ^^^^^^^^^^^^^^^^^^^^^^^^^^
" FIXME:
" 	Same stupidness here as in EXPR=, but it works.
" 
syn region iHTMLSql
	\ contained 
	\ transparent
	\ keepend
	\ start=+\sSQL\s*=\s*\z(["'`]\)+
	\ end=+\z1+
	\ contains=iHTMLEqual,iHTMLVar,iHTMLEnvVar,iHTMLSysVar,iHTMLAllQuotes,iHTMLTag,iHTMLSqlAttrib,@iHTMLAddSql
syn keyword iHTMLSqlAttrib SQL

" attribute, everything before the '='
"
" EXAMPLE:
"
" <iDATE FMT="%B">
"        ^^^
"
syn keyword iHTMLAttrib A ADDRESS ADJUST AFTER ALIAS AMOUNT ARGS ASC ATTACH  contained
syn keyword iHTMLAttrib B BCC BGB BGG BGR BLUE BORDER BREAKONFAILURE BREAKONNOOUTPUT BRUSH BTWN BUSY   contained
syn keyword iHTMLAttrib CACHE CAPTURE CASE CC CCMEXP CCNUM CCYEXP CHR CITY CLEAR CLIENT CMD COLOR COLUMNS	COMMENTS COND COOKIE COUNTRY CREATE CURRENCY  contained
syn keyword iHTMLAttrib DATA DATE1 DATE2 DATE DAY DBNAME DELETE DELIM DIRECTION DIRFAIL DISCARD DLL DRIVER DST DSN DSTNAME  contained
syn keyword iHTMLAttrib EHANDLE EMPTY END ESC EVAL EXPR EXT EXPIRES  contained
syn keyword iHTMLAttrib FAILURE FILENAME FILE FILT FINAL FIRST FLAGS FMT FONT FROM FUNCTION  contained
syn keyword iHTMLAttrib GLOBAL GREEN  contained
syn keyword iHTMLAttrib H1 H2 HEADERS HEAP HEIGHT HIGH HOST HOUR  contained
syn keyword iHTMLAttrib ID INDEX INDEXED INFO INITIAL INTERLACE INTERVAL ITEM  contained
syn keyword iHTMLAttrib KEEP KEY KILL  contained
syn keyword iHTMLAttrib LEN LIST LISTNAME LOCALPATH LOGIN LOW  contained
syn keyword iHTMLAttrib MASK MAX MAXHOPS MIN MODE MODES MONTH MUTEX  contained
syn keyword iHTMLAttrib NAME NEWNAME NEWTEXT NOOUTPUT NOTALIAS NOTCOND NUM NUMBERS  contained
syn keyword iHTMLAttrib O OBJECT ODBC OMIT OP ORGANIZATION OUTPUT OUTVAR  contained
syn keyword iHTMLAttrib PAD PARSE PASS PATH PORT POST PREC PRECISION PRIORITY PROCNAME PWORD  contained
syn keyword iHTMLAttrib QUALIFIER QUALITY QUOTE  contained
syn keyword iHTMLAttrib RED REGEX REPLYTO REMOTEPATH REPLACE RESOLVE RESTART RESULT RESULT  contained
syn keyword iHTMLAttrib SEC SECHOST SECRET SEED SENDER SEP SERVER SQL SRC SRCNAME START STATE STDIN STEP STOP STORE STYLE SUBJECT SUBST  contained
syn keyword iHTMLAttrib TABLE TABLENAME TEXT TILE TIME1 TIME2 TIME TIMEOUT TO TYPE TYPES  contained
syn keyword iHTMLAttrib UID URL USER  contained
syn keyword iHTMLAttrib VALUE VAR VERSION  contained
syn keyword iHTMLAttrib W1 W2 WAITLOCK WIDTH  contained
syn keyword iHTMLAttrib X1 X2 X  contained
syn keyword iHTMLAttrib Y1 Y2 Y YEAR  contained
syn keyword iHTMLAttrib ZIP  contained

syn keyword iHTMLTagName iABS iADDHEADER iASC iAUTHOR  contained
syn keyword iHTMLTagName iBASECONV iBIT  contained
syn keyword iHTMLTagName iCASE iCEIL iCGI iCHR iCLASSC iCLASSC iCLEAN iCLEAR iCLEAR iCONTENT iCOOKIE iCOPYFILE iCOUNTER iCRDATE iCRDATETIME iCRTIME   contained
syn keyword iHTMLTagName iDATE iDATE iDATEDIFF iDATEEXT iDATETIME iDATETIME iDEFAULT iDIR iDOTDOT iDOTDOT iDOWNLOAD iDSN iDSNCONFIG iDSN iDTSTRUCT  contained
syn keyword iHTMLTagName iELSE iELSEIF iEQ iERROR iEVAL iFILE iFILEINFO iFLOOR iFORMAT iFORMAT iFTP iFTPACTION iFUNCCALL iFUNCTION contained
syn keyword iHTMLTagName iGETMIMEFILE iGETMIMENAME iGMTIME iGMTIME  contained
syn keyword iHTMLTagName iHEAPDUMP iHEAPDUMP iHTML iHTMLDECODE iHTTP iHTX  contained
syn keyword iHTMLTagName iIF iIMAGEARC iIMAGECLEARCOLOR iIMAGECOPY iIMAGECREATE iIMAGEDESTROY iIMAGEFILL iIMAGEFROMFILE iIMAGEGETCOLOR iIMAGEGETCOMPONENT iIMAGEGETINFO iIMAGEGETPIXEL iIMAGELINE iIMAGERECT iIMAGESETBRUSH iIMAGESETINTERLACE iIMAGESETPIXEL iIMAGESETSTYLE iIMAGESETTRANSPARENT iIMAGESTRETCH iIMAGETEXT iIMAGEWITHIN iIMAGEWRITE iIMGSIZE iINCLUDE iIP iIP iISALPHA iISDATE iISDEF iISDIGIT iISLEAP iISTIME  contained
syn keyword iHTMLTagName iLISTCREATE iLISTGET iLISTINSERT iLINK iLOOP  contained
syn keyword iHTMLTagName iMAIL iMATH iMAX iMIN iMD5  contained
syn keyword iHTMLTagName iNAP iNUMTEXT  contained
syn keyword iHTMLTagName iPAY iPEEK iPEEK iPGP iPING iPOP iPOPFETCH iPOPFETCH iPOPHEADER iPRINTF iPUSH iPUSH  contained
syn keyword iHTMLTagName iRANDOM iREADLINE iREDIR iREGEXFIND iREGEXLIST iREGEXREPLACE iREGKEY iREGVAL iREPLACE iRNDNUM iREM iREMBLOCK iRESUME  contained
syn keyword iHTMLTagName iSCHEDULE iSET iSQL iSQLCOLUMNS iSQLFETCH iSQLFETCH iSQLINFO iSQLMORE iSQLNEXTSET iSQLPROCCOLUMNS iSQLPROCS iSQLROW iSQLTABLES iSRVR iSTOP iSTOP iSTRCAT iSTRCMP iSTRCSPAN iSTRICMP iSTRIN iSTRJUST iSTRKEEP iSTRLEFT iSTRLEN iSTRLPAD iSTRLWR iSTRNCMP iSTRNICMP iSTRREM iSTRREV iSTRRIGHT iSTRRPAD iSTRSPAN iSTRTRIM iSTRUPR iSVC iSWITCH  contained
syn keyword iHTMLTagName iTEMPNAME iTHREADS iTICKS iTIME iTIME iTIMEDIFF iTIMESLOT  contained
syn keyword iHTMLTagName iUNDEFINE iUNDEFINESET iURLENCODE  contained
syn keyword iHTMLTagName iWHILE iWRITELINE  contained
syn keyword iHTMLTagName iXCOPY iXDELETE iXFIND  contained

" start tag
"
" EXAMPLE:
"
" <iIF ALIAS="one" COND="blah">
" s^^^^^^^^^^^^^^^^^^^^^^^^^^^e
"
syn region   iHTMLTag
    \ start=+<i[^ /!?<>"']\@=+
    \ end=+>+
    \ contains=iHTMLTagName,iHTMLAttrib,iHTMLEqual,iHTMLString,iHTMLExpression,iHTMLSql

" HTML tags that look like iHTML tags due to the leading "<i" should not be
" highlighted
syn region iHTMLNotATag
    \ start=+<\(img\|input\)\@=+
    \ end=+>+
    \ display

" end tag
"
" EXAMPLE:
"
" </iIF ALIAS="asdf">
" ss^^^^^^^^^^^^^^^^e                
"
syn region   iHTMLEndTag
    \ start=+</i[^ /!?<>"']\@=+
    \ end=+>+
    \ contains=iHTMLEndTagName,iHTMLEndTagAttrib,iHTMLEqual,iHTMLString

" HTML tags that look like iHTML tags due to the leading "</i" should not be
" highlighted
syn region iHTMLNotAnEndTag
    \ start=+</\(img\|input\)\@=+
    \ end=+>+
    \ display

" valid iHTML end tag names and attributes
" 
" EXAMPLE:
"
" </iIF ALIAS="asdf">
"   ^^^ #####
"  
syn keyword iHTMLEndTagName	iCASE iDBLOOP iDBQUERY iDBRESULTS iDBTABLE iFTP iFUNCTION iIF iPORT iSQL iSQLTRANS iSWITCH iTELNET iWHILE contained
syn keyword iHTMLEndTagAttrib	ALIAS EHANDLE contained

" environment variables
syn keyword iHTMLEnvVar	i_auth_pass i_auth_type i_auth_user i_browser i_content_length i _error i_errortext i_http_from i_ip i_loop i_method i_path i_path_info i_port i_query_string i_referer i_server_extension i_server_protocol i_server_software i_sqlempty i_sqlerrortext i_badtag i_content_type i_cookie_path i_cookie_life i_currentpage i_diedat i_DSD i_errordetail i_fdate i_hinttext i_os i_pop_size i_prevpage i_remote_host i_servername i_sqlerrorstmt i_timestamp i_vpath contained 
" TODO: problem here with matching environment vars followed by end-of-line
syn match iHTMLEnvVar +i_priv_\w\++
syn match iHTMLEnvVar +i_http_\w\++

" iHTML variables
"
" EXAMPLE:
"
" :my_variable (regular colon variable)
" :i$crlf      (system variable - note I make a distinction between these and
"               environment variables)
" NOTES:
"   We support ::thing style variables for EVAL="true" type situations
"
syn match   iHTMLVar                    +:\{1,2}\w\++ contains=iHTMLVarPunct,iHTMLEnvVar
syn match   iHTMLVarPunct     contained +:+
syn match   iHTMLSysVar                 +:i\$\(crlf\|cr\|lf\|tab\|space\|ff\|esc\|pi\)+ contains=iHTMLSysVarPunct

" iHTML Comments
"
" EXAMPLE:
"
" <iREM -- this is a comment --> <iREM as is this>
" <iREMBLOCK>
" 	blocks are handy
" </iREMBLOCK>
"
syn region  iHTMLComment start=+<iREM+ end=+>+
syn keyword iHTMLTodo         TODO FIXME XXX
syn region iHTMLCommentBlock start=/<iREMBLOCK/ end=/<\/iREMBLOCK>/ fold

" Number (integers)
syn match iHTMLNumber	"-\=\<\d\+\>"	contained

" Float (floating point/decimal)
syn match iHTMLFloat	"\(-\=\<\d+\|-\=\)\.\d\+\>"	contained

" Operator
syn match	iHTMLOperator	"[-+%^|~&*!()]"	contained
syn match	iHTMLOperator	"\<and\>\|\<or\>\|\<xor\>\|\<not\>"	contained
syn keyword	iHTMLOperator	PLUS MINUS TIMES DIV LT GT EQ EQUAL EQUALS IS P POW POWTEN NE NEQ != # C COS S SIN T TAN N LN MOD NOT COMPLEMENT K GE GTE L LE LTE E EXP ETOX ACOS ACS ASIN ASN ATAN ATN G LOG contained

" The iHTMLAllQuotes group is a bit of a hack to make quotes highlight
" nicely in weird situations like iHTMLExpression (basically, anywhere special
" rules exist within strings after certain tag attributes)
syn match	iHTMLAllQuotes +['"`]+ contained


" 'bang' iHTML tags (start and end)
syn region iHTMLBang
	\ matchgroup=iHTMLTag
	\ start=+</\=!iHTML+rs=e-5
	\ end=+>+re=e-1


"
" Foldable regions 
" I've chosen to to allow folding of everything, since that's just overkill
" and will slow things down.  It's easy to add more regions if you're so
" inclined. Note that iREMBLOCK is already fold-able.
"
syn region iHTMLFoldable start=/<script[^>]*>/ end=/<\/script[^>]*>/ keepend extend fold transparent
syn region iHTMLFoldable start=/<iIF[^>]*>/ end=/<\/iIF[^>]*>/ keepend extend fold transparent
syn region iHTMLFoldable start=/<iSWITCH[^>]*>/ end=/<\/iSWITCH[^>]*>/ keepend extend fold transparent
syn region iHTMLFoldable start=/<iCASE[^>]*>/ end=/<\/iCASE[^>]*>/ keepend extend fold transparent
syn region iHTMLFoldable start=/<iWHILE[^>]*>/ end=/<\/iWHILE[^>]*>/ keepend extend fold transparent
syn region iHTMLFoldable start=/<iLOOP[^>]*>/ end=/<\/iLOOP[^>]*>/ keepend extend fold transparent

" synchronizing
" FIXME: make sync sensible
syn sync fromstart

" The default highlighting.
hi def link iHTMLTodo		Todo

" tags and attributes
hi def link iHTMLTag		Function
hi def link iHTMLTagName	Statement
hi def link iHTMLEndTag		Function
hi def link iHTMLEndTagName	Statement

hi def link iHTMLAttrib		Type
hi def link iHTMLEndTagAttrib	Type
hi def link iHTMLExpressionAttrib	Type
hi def link iHTMLSqlAttrib	Type

" colon/env/system variables
hi def link iHTMLVar		Identifier
hi def link iHTMLEnvVar		Type
hi def link iHTMLVarPunct	Delimiter
hi def link iHTMLSysVar		Type
hi def link iHTMLSysVarPunct	Operator

" the basics: strings, numbers, operators, etc.
hi def link iHTMLEqual		Function
hi def link iHTMLAllQuotes	Delimiter
hi def link iHTMLQuote		Delimiter
hi def link iHTMLString		String
hi def link iHTMLNumber		Number
hi def link iHTMLFloat		Float
hi def link iHTMLOperator	Operator

" comments
hi def link iHTMLComment	Comment
hi def link iHTMLCommentBlock	Comment

" misc stuff
hi def link iHTMLBang		Special

let b:current_syntax = "ihtml"

let &cpo = s:iHTML_cpo_save
unlet s:iHTML_cpo_save

" vim: ts=8

