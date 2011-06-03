" Vim syntax file
" Language:	jBASE jBASIC source code
" Maintainer:	Gary Calvin <gcalvin@kenwoodusa.com>
" Filenames:    *.b
" Last Change:	10th April 2004
" URL:		<TBA>
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" jBASIC keywords
syn case match
syn keyword jbaseConditional   BEGIN CASE DO ELSE END ERROR FOR FROM GO GOSUB GOTO IF LOOP NEXT ON REPEAT THEN TO UNTIL WHILE

syn keyword jbaseFunction      @ ABS ABSS ADDS ALPHA ANDS ASCII ASSIGNED BITCHANGE BITCHECK BITRESET BITSET
syn keyword jbaseFunction      BYTELEN CATS CHANGETIMESTAMP CHAR CHARS CHDIR CHECKSUM COL1 COL2 COMPARE CONVERT
syn keyword jbaseFunction      COS COUNT COUNTS DATE DCOUNT DECRYPT DIV DIVS DOWNCASE DQUOTE DROUND DTX DYNTOXML
syn keyword jbaseFunction      EBCDIC ENCRYPT EQS EREPLACE EXIT EXP EXTRACT FIELD FIELDS FILEINFO GES GETCWD
syn keyword jbaseFunction      GETENV GETUSERGROUP GROUP ICONV ICONVS IFS INDEX INMAT INSERT INT ISALPHA ISALNUM
syn keyword jbaseFunction      ISDIGIT ISPRINT ISCNTRL ISSPACE ISLOWER ISUPPER JBASECOREDUMP JBASETHREADCreate
syn keyword jbaseFunction      JBASETHREADStatus ITYPE KEYIN LATIN1 LEFT LEN LENS LENDP LES LN LOCALDATE
syn keyword jbaseFunction      LOCALTIME LOWCASE LOWER MAKETIMESTAMP MATCHFIELD MOD MODS MULS NEGS NES NOTS
syn keyword jbaseFunction      NUM NUMS OCONV OCONVS ORS PUTENV PWR QUOTE RAISE RECORDLOCKED REPLACE RIGHT RND
syn keyword jbaseFunction      SADD SDIV SENTENCE SEQ SEQS SIN SMUL SORT SOUNDEX SPACE SPACES SPLICE SPOOLER
syn keyword jbaseFunction      SQRT SQUOTE SSUB STR STRS SUM SWAP SYSTEM SUBS SUBSTRINGS TAN TIME TIMEDATE
syn keyword jbaseFunction      TIMEDIFF TIMESTAMP TRANS TRIM TRIMB TRIMBS TRIMF TRIMFS UNASSIGNED UNIQUEKEY
syn keyword jbaseFunction      UPCASE UTF8 XLATE XMLTODYN XMLTOXML XTD
syn keyword jbaseFunction      JQLCOMPILE JQLEXECUTE JQLFETCH JQLGETPROPERTY JQLPUTPROPERTY

syn keyword jbaseOperator      AND EQ GT LT MATCH MATCHES NE NOT OFF ON OR

syn keyword jbaseStatement     ABORT BEFORE BREAK CALL CALLC CALLDOTNET CALLJ CALLONEXIT CAPTURING CHAIN CHANGE CLEAR
syn keyword jbaseStatement     CLEARCOMMON CLEARDATA CLEARFILE CLEARINPUT CLEARSELECT CLOSE CLOSESEQ COLLECTDATA COMMON
syn keyword jbaseStatement     CONTINUE CONVERT CREATE CRT DATA DEBUG DEFC DEFCE DEFFUN DEL DELETE DELETELIST
syn keyword jbaseStatement     DELETESEQ DELETEU DIM DIMENSION ECHO ENTER EQU EQUATE EXECUTE EXIT FILELOCK
syn keyword jbaseStatement     FILEUNLOCK FIND FINDSTR FLUSH FMT FMTS FOLD FOOTING FORMAT FORMLIST FUNCTION
syn keyword jbaseStatement     GET GETLIST GETX HEADING HEADINGE HEADINGN HUSH IF IN INPUT INPUTNULL INS
syn keyword jbaseStatement     LOCATE LOCK MAT MATBUILD MATPARSE MATREAD MATREADU MATWRITE MATWRITEU MSLEEP
syn keyword jbaseStatement     NOBUFF NULL OBJEXCALLBACK OPEN OPENDEV OPENPATH OPENSEQ OPENSER OSBREAD OSBWRITE
syn keyword jbaseStatement     OSCLOSE OSDELETE OSOPEN OSREAD OSWRITE OUT PAGE PAUSE PASSLIST PCPERFORM PERFORM
syn keyword jbaseStatement     PRECISION PRINT PRINTER PRINTERR PROCREAD PROCWRITE PROGRAM PROMPT READ READBLK
syn keyword jbaseStatement     READL READLIST READNEXT READSELECT READSEQ READT READU READV READVL READVU READXML
syn keyword jbaseStatement     RELEASE REMOVE RETURN RETURNING REWIND RQM RTNDATA RTNLIST SEEK SELECT SETTING SEND SENDX
syn keyword jbaseStatement     SLEEP SSELECT SSELECTN SSELECTV STOP SUBROUTINE TIMEOUT UNLOCK UDTEXECUTE WAKE WEOF
syn keyword jbaseStatement     WEOFSEQ WRITE WRITEBLK WRITELIST WRITESEQ WRITET WRITEU WRITEV WRITEVU WRITEXML

"integer number, or floating point number without a dot.
syn match  jbaseNumber          "\<\d\+\>"
"floating point number, with dot
syn match  jbaseNumber          "\<\d\+\.\d*\>"
"floating point number, starting with a dot
syn match  jbaseNumber          "\.\d\+\>"

" operators
syn match jbaseOperator        "\s[-+\*/%]\s"
syn match jbaseOperator        "="
syn match jbaseOperator        "[-+\*/%]="
syn match jbaseOperator        "-\|=\|[:()\[\]<>+\*^/\\]"

" String delimiters
syn region jbaseString         start=+"+ end=+"+
syn region jbaseString         start=+'+ end=+'+
syn region jbaseString         start=+/+ end=+/+

syn region jbaseComment        start="^\s*\*" end="$"
syn region jbaseComment        start=";\s*\*" end="$"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_jbase_syntax_inits")
  if version < 508
    let did_jbase_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink jbaseFunction		Statement
  HiLink jbaseStatement		Statement
  HiLink jbaseConditional	Conditional

  HiLink jbaseOperator		Operator

  HiLink jbaseString		String
  HiLink jbaseNumber		Number

  HiLink jbaseComment		Comment

  delcommand HiLink
endif

let b:current_syntax = "jbase"

" vim: ts=4 sw=4
