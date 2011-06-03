" Vim syntax file - Speedware
" TODO: Make sure everything works
" Language:	Speedware
" Maintainer:	Patrick Regan <patrick.rubbs.regan@gmail.com>
" Updated:	2009-11-13 Initial publish of syntax file
" Notes: This is a syntax file for the Speedware and Autobahn programming
" languages. I maintain this on my own time to make my life easier at work.
" This file is licensed in the same way Vim is. It is encouraged to make
" changes and share them. If you have questions please email the address
" above.

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

set autoindent

set iskeyword +=-

"$Statments. Does not check to see if valid, matches all words with a '$' in
"front.
syn match		speedwareStatement		"\$[A-Z]\+"
syn match		speedwareStatement		"\$[A-Z]\+_[A-Z]\+"

"Verbs
syn keyword speedwareVerb         ABORT ACCEPT AT BRANCH BREAK BUTTON CALCUL
syn keyword speedwareVerb         CALL CALLSUB CASE CLOSE COMMAND CONTINUE
syn keyword speedwareVerb         CREATE CRITERIA DCL DCLDDE DCLFILE DCLFRAME
syn keyword speedwareVerb         DCLHUGE DCLPRINTER DCLPROC DCLSESSION 
syn keyword speedwareVerb         DCLSTRUCT DCLVIEW DELETE DOMAIN ENDSUB ERR
syn keyword speedwareVerb         EXEC EXPORT FAIL FIELD FILE FNCKEY FOR GO IF
syn keyword speedwareVerb         INP LET LINK LIST LOOP MENUBAR MENUITEM
syn keyword speedwareVerb         MENUPANEL MENUSEPARATOR MODE MSG MSGBOX NEW
syn keyword speedwareVerb         NOTHING ON OPEN PARM PARMHUGE PASSWORD PAUSE
syn keyword speedwareVerb         PICTURE PRINT PROCEED PROTECTED REMOTE RETRY
syn keyword speedwareVerb         RETURN RUN SECTION SEGMENT SELECT SET SKIP
syn keyword speedwareVerb         SORT SORT SORTED SUBMIT SUBRETURN SUBSECTION
syn keyword speedwareVerb         TAKE TRANSACTION WAIT WHILE 

"Keywords
syn keyword speedwareKeyword      BEGIN END DO EXIT 

"Keywords that are not listed in Ref docs so they are not in any particular
"order *sorry*
syn keyword speedwareKeyword      FROM LENGTH SUBTYPE OF AND OR NOSEQ
syn keyword speedwareKeyword      SPACES NOT FINISH TERMINATE GLOBAL
"syn keyword speedwareKeyword      

"Options
syn keyword speedwareOption       ACCESS ALIAS APPLICATION ARITHMETIC ASSIGN
syn keyword speedwareOption       ASYNC BACKWARD BASE BATCHJOB BOTTOM
syn keyword speedwareOption       BRANCH-ENTRY CENTER COMMIT-COUNT COMPATIBLE
syn keyword speedwareOption       COMPILEINFO CONDITION CONNECT CONNECTION
syn keyword speedwareOption       CONTROL-Y COUNTRY CURSOR DDEERROR DDEITEM
syn keyword speedwareOption       DDESERVER DEFAULT DISPLAY DUP EDIT ENCRYPT
syn keyword speedwareOption       ENDPROGRAM ENVFILE ERROR EXECUTE EXECUTEINFO
syn keyword speedwareOption       FIELD-ENTRY FILES FORM FORMAT FREEZE GET HEAD
syn keyword speedwareOption       HELP HILITE HOST IGNORE IMAGE-LOCK IN INIT 
syn keyword speedwareOption       INPUT INPUT-STACKING IS ITEM-AREA ITERATIONS
syn keyword speedwareOption       KEY LABEL LANGUAGE LAUNCH LEFT LIKE LIST 
syn keyword speedwareOption       LISTING LIST-SELECTED LOCATION LOGON LOOKUP
syn keyword speedwareOption       MANDATORY MANUAL MATCH MENUCASCADE MENUNABLE
syn keyword speedwareOption       MENUPOPUP MENURADIO MENUTOGGLE MODIFY
syn keyword speedwareOption       MSGOPTIONS MSGSTYLE NAME NEED NEWDEVICE NEXT
syn keyword speedwareOption       NOENTRY NOKEY NOLABEL NORUN SOSEQ NOWARN 
syn keyword speedwareOption       OMNI-CRITERIA OMNIDEX ONLY OPEN-IMMEDIATE
syn keyword speedwareOption       OUT PICTURE-AREA PRINTER PRINTSCREEN PROMPT
syn keyword speedwareOption       RAW RECOMPILE REDEFINES REPEAT REPORT-SETUP
syn keyword speedwareOption       RESET RETURN-VALUE RIGHT SCOPE SCREEN-ENTRY
syn keyword speedwareOption       SCREEN-REPAINT SELECTED SEQUENCE SHAPE SHOW
syn keyword speedwareOption       SKIP SORT SOUND START STATEMENT STEP STORE
syn keyword speedwareOption       STRUCT SYNC TAKEFILE TEXT TIMEOUT TITLE TOP
syn keyword speedwareOption       TYPE USER VAL WAIT WINDOW WINDOW-LIST 
syn keyword speedwareOption       WINDOW-SELECT WINDOW-SORT 

"Clause
syn keyword speedwareClause        AS COLUMNS GET-INFORMATION INTO-VARIABLES
syn keyword speedwareClause        RETURN-VALUE SET-INFORMATION USING WHERE 
syn keyword speedwareClause        WITH

"Conditionals
syn keyword speedwareConditional	IF THEN ELSE CASE ENDCASE NULL OTHERWISE

"Looping
syn keyword speedwareRepeat				LOOP FOR 

"Types
syn keyword speedwareType					INT ALPHA NUMERIC FLOAT CHAR DECIMAL 

"Specials.
syn keyword speedwareSpecial			UP BLACK CYAN HALF-INV INVERSE 
syn keyword speedwareSpecial			QUESTION INVISIBLE
syn region	speedwareSpecial			start=+'+ end=+'+ 

"Subsection calls, may be unnecessary due to others, but here for completeness
syn keyword	speedwareStatement		SUBSECTION nextgroup=speedwareFunction skipwhite

"Function idendifiers (same as normal identifiers but here for completeness)
syn match		speedwareFunction			/\h\+\(\w|\-\)*/ "/\w\@<!\h\w*\w\@!/

"String constants
syn region	speedwareString				start=+"+ end=+"+ 

"Booleans
syn keyword speedwareBoolean			TRUE FALSE

"Identifiers
syn match		speedwareIdentifier		/\h\+\(\w|\-\)*/ "/\w\@<!\h\w*\w\@!/

"Include statements
syn match 	speedwareInclude			"#.*$"

"Number constants
syn match		speedwareNumber				"\d\+"
syn match		speedwareNumber				"\d\+\.\d\*"

"Comments
syn region	speedwareComment			start="(\*" end="\*)" contains=speedwareTODO keepend

"TODO's and Program name Identifiers in a comment section
syn match		speedwareTODO					"^\s*TODO.*$" contained
syn match		speedwareTODO					"^\s*REPORT.*$" contained
syn match		speedwareTODO					"^\s*UTILITY.*$" contained
syn match		speedwareTODO					"^\s*INCLUDE.*$" contained
syn match		speedwareTODO					"^\s*SCREEN.*$" contained
syn match		speedwareTODO					"^\s*LOGIC.*$" contained

let b:current_syntax = "spw"

hi def link speedwareNumber				Constant
hi def link speedwareBoolean			Constant
hi def link	speedwareStatement		Statement
hi def link speedwareVerb         Statement
hi def link speedwareKeyword      Keyword
hi def link speedwareOption       Statement
hi def link speedwareClause       Statement
hi def link speedwareIdentifier		Identifier
hi def link speedwareFunction			Identifier
hi def link speedwareRepeat				Statement
hi def link speedwareConditional	Statement
hi def link speedwareType					Type
hi def link speedwareSpecial			Special
hi def link speedwareString				Constant
hi def link speedwareComment 			Comment
hi def link speedwareTODO					TODO
hi def link speedwareInclude			PreProc
