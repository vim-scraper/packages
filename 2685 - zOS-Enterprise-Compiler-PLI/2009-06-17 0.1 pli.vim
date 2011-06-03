" Vim syntax file
" Language:	pli 
" Maintainer:	Ing. Michael Fitz
" Last change:	2009-06-17
" Version: 0.1 (Based on Ing. Michael Fitz's version) Updated for z/OS PLI Enterprise Compiler

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

set isk+=#

" PLI is a case insensitive language
syn case ignore
setlocal ignorecase

syn keyword pliTodo contained	TODO FIXME XXX

" String
syn region  pliString		start=+'+  end=+'[bx]\=+

syn match   pliOperator		"[=!\+\-\*\/\<\>\^|&,;]"

syn match   pliIdentifier	"\<[a-zA-Z_#][a-zA-Z0-9_#]*\>"

syn match   pliDelimiter	"[()\.]"

syn match   pliStart            '\%1v[^0-9 -]'

"syn match   pliSeptation        '\%73c.'

syn region  pliPreProc		start="%" end=";" contains=pliComment,pliString

" FIXME : No Number support for floats, as I'm working on an embedded
" project that doesn't use any.
syn match   pliNumber		"-\=\<\d\+\>"
"syn match   pliNumber		"\<[0-9a-fA-F]*[hH]*\>"

"If you don't like tabs
"syn match pliShowTab "\t"
"syn match pliShowTabc "\t"

"when wanted, highlight trailing white space
if exists("c_space_errors")
  syn match	pliSpaceError	"\s*$"
  syn match	pliSpaceError	" \+\t"me=e-1
endif

" Use the same control variable as C language for I slieve
" users will want the same behavior
syn match       pliCommentStarter "/\*" contained 
syn region	pliComment	start="/\*" end="\*/" 
syn region	pliComment	start="^ \+\*" end="$" 

"syntax match  pliLabel "\<[a-zA-Z_#][a-zA-Z0-9_#]*\>:" 
syntax match  pliProcedure "\s\+\w\+\s*:\s*\(PROC\s*;\)\?" 
syntax match  pliProcedure "\s\+END\s\+\w\+\s*;"
syntax region pliIncludeFile start="%INCLUDE" end=";"

syntax match  pliCommentError	"\*/"

syn region pliSQL_Statement start="EXEC\_s\+SQL" end=";" contains=pliComment,pliString
syn region pliCICS_Statement start="EXEC\_s\+CICS" end=";" contains=pliComment,pliString

syn keyword pliType     BIN[ARY] BIT CHAR DEC[IMAL] ENTRY FIXED FLOAT PIC[TURE] 
syn keyword pliType     AUTO BASED PTR UNAL[IGNED] VAR[YING] STATIC 

syn keyword pliReserved DECLARE DCL BUILTIN REPEAT 
syn keyword pliReserved CALL GO GOTO EXTERNAL CTL CONTROLLED REFER
syn keyword pliReserved REENTRANT RETURN[S] REORDER ORDER PROC OPTIONS MAIN PRINT 
syn keyword pliReserved FILE STREAM INPUT OUTPUT PUT SKIP LIST DATA ENV BLKSIZE EDIT
syn keyword pliReserved BY IF THEN ELSE WHILE DO TO END UNTIL SELECT WHEN OTHER[WISE]
syn keyword pliReserved ALLOC FREE DELAY LIKE 
syn keyword pliReserved ON ERROR SNAP BEGIN SYSTEM

syn keyword pliBuiltin ABS ACOS ACOSF ADD ADDR ADDRDATA INIT[IAL] VALUE
syn keyword pliBuiltin ALL ALLOCATE ALLOCATION ALLOCSIZE ANY ASIN 
syn keyword pliBuiltin ASINF ATAN ATAND ATANF ATANH AUTOMATIC AVAILABLEAREA 
syn keyword pliBuiltin BINARYVALUE BITLOCATION BOOL BYTE 
syn keyword pliBuiltin CDS CEIL CENTERLEFT CENTRELEFT CENTERRIGHT 
syn keyword pliBuiltin CENTRERIGHT CHARACTER CHARGRAPHIC CHARVAL 
syn keyword pliBuiltin CHECKSTG COLLATE COMPARE COMPLEX CONJG COPY COS COSD 
syn keyword pliBuiltin COSF COSH COUNT CS CURRENTSIZE CURRENTSTORAGE CSTG
syn keyword pliBuiltin DATAFIELD DATE DATETIME DAYS DAYSTODATE DAYSTOSECS 
syn keyword pliBuiltin DIM[ENSION] DIVIDE 
syn keyword pliBuiltin EMPTY ENDFILE ENTRYADDR EPSILON ERF ERFC EXP EXPF EXPONENT 
syn keyword pliBuiltin FILEDDINT FILEDDTEST FILEDDWORD FILEID FILEOPEN 
syn keyword pliBuiltin FILEREAD FILESEEK FILETELL FILEWRITE FLOOR 
syn keyword pliBuiltin GAMMA GETENV GRAPHIC 
syn keyword pliBuiltin HANDLE HBOUND HEX HEXIMAGE HIGH HUGE 
syn keyword pliBuiltin IAND IEOR IMAG INDEX INOT IOR ISIGNED ISLL ISMAIN ISRL IUNSIGNED 
syn keyword pliBuiltin LBOUND LEFT LENGTH LINENO LOCATION LOG LOGF 
syn keyword pliBuiltin LOGGAMMA LOG2 LOG10 LOG10F LOW LOWERCASE LOWER2 
syn keyword pliBuiltin MAX MAXEXP MAXLENGTH MIN MINEXP MOD MPSTR MULTIPLY NULL 
syn keyword pliBuiltin OFFSET OFFSETADD OFFSETDIFF OFFSETSUBTRACT OFFSETVALUE  
syn keyword pliBuiltin OMITTED ONCHAR ONCODE ONCONDCOND ONCONDID ONCOUNT ONFILE 
syn keyword pliBuiltin ONGSOURCE ONKEY ONLOC ONSOURCE ONSUBCODE ONWCHAR 
syn keyword pliBuiltin ONWSOURCE ORDINALNAME ORDINALPRED ORDINALSUCC 
syn keyword pliBuiltin PACKAGENAME PAGENO PLACES 
syn keyword pliBuiltin PLIASCII PLICANC PLICKPT PLIDELETE PLIDUMP PLIEBCDIC
syn keyword pliBuiltin PLIFILL PLIFREE PLIMOVE PLIOVER PLIREST PLIRETC
syn keyword pliBuiltin PLIRETV PLISAXA PLISAXB PLISRTA PLISRTB PLISRTC PLISRTD 
syn keyword pliBuiltin POINTER POINTERADD POINTERDIFF POINTERSUBTRACT POINTERVALUE
syn keyword pliBuiltin POLY PRECISION PRED PRESENT PROCEDURENAME PROD PUTENV PTRADD
syn keyword pliBuiltin RADIX RAISE2 RANDOM RANK REAL REM REPATTERN 
syn keyword pliBuiltin REPEAT REVERSE RIGHT ROUND 
syn keyword pliBuiltin SAMEKEY SCALE SEARCH SEARCHR SECS SECSTODATE 
syn keyword pliBuiltin SECSTODAYS SIGN SIGNED SIN SIND SINF SINH SIZE 
syn keyword pliBuiltin SOURCEFILE SOURCELINE SQRT SQRTF STORAGE STG 
syn keyword pliBuiltin STRING SUBSTR SUBTRACT SUCC SUM SYSNULL SYSTEM SYSPRINT PICSPEC 
syn keyword pliBuiltin TALLY TAN TAND TANF TANH THREADID TIME TINY 
syn keyword pliBuiltin TRANSLATE TRIM TRUNC TYPE 
syn keyword pliBuiltin UNALLOCATED UNSIGNED UNSPEC UPPERCASE 
syn keyword pliBuiltin VALID VALIDDATE VARGLIST VARGSIZE VERIFY VERIFYR 
syn keyword pliBuiltin WCHARVAL WEEKDAY WHIGH WIDECHAR WLOW 
syn keyword pliBuiltin Y4DATE Y4JULIAN Y4YEAR 
syn keyword pliBuiltIn NOFOFL CENT CHKEOM CHKMNT2 CHKMNTH CHKNUM 
syn keyword pliBuiltIn CHKSIGN DTFORMAT DUMP GREGJUL RUCHKNUM TIMEDIFF
" Folgende BUILTINS sind auch TYPES: FIXED FLOAT EDIT BINARY DECIMAL BIT POINTER 

syn sync lines=50

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_pli_syntax_inits")
  if version < 508
    let did_pli_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

 " The default methods for highlighting.  Can be overridden later
  HiLink pliLabel			Label
  HiLink pliProcedure			Label
"  HiLink pliConditional		Conditional
"  HiLink pliRepeat			Repeat
  HiLink pliTodo			Todo
  HiLink pliNumber			Number
  HiLink pliOperator			Operator
  HiLink pliDelimiter			Delimiter
  "HiLink pliShowTab			Error
  "HiLink pliShowTabc			Error
  HiLink pliIdentifier			Identifier
  HiLink pliBuiltIn			Builtin
  HiLink pliReserved			Statement
  HiLink pliPreProc			PreProc
  HiLink pliCommentError		Error
  HiLink pliString			String
  HiLink pliComment			Comment
  HiLink pliType                        Type
  HiLink pliIncludeFile                 Include
  HiLink pliSQL_Statement               SQL_Statement
  HiLink pliCICS_Statement              CICS_Statement
  HiLink pliSeptation                   Error
  HiLink pliStart                       Error            

  delcommand HiLink
endif

let b:current_syntax = "pli"
" vim: ts=8 sw=2

