" Vim ABAP syntax file Ver: 0.5
"    Language:	SAP - ABAP/4
"  Maintainer:	Marius van Wyk <marius@e.co.za>
" Last Change:	2004.08.25

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Always ignore case
syn case ignore

" Symbol Operators
syn match   abapSymbolOperator      "[+\-/=<>]"
syn match   abapSymbolOperator      "\*"
syn match   abapSymbolOperator      "[<>]="
syn match   abapSymbolOperator      "<>"
syn match   abapSymbolOperator      "\*\*"
syn match   abapSymbolOperator      "[()]"
syn match   abapSymbolOperator      "[:,\.]"

" Literals
syn region  abapString matchgroup=abapString start="'" end="'" contains=abapStringEscape
syn match   abapStringEscape contained "''"

syn match   abapNumber	"-\=\<\d\+\>"
syn region  abapHex     matchgroup=abapHex start="X'" end="'"

if version >= 600
  setlocal iskeyword=-,48-57,_,A-Z,a-z
else
  set iskeyword=-,48-57,_,A-Z,a-z
endif

" ABAP statements
syn keyword abapStatement ADD ADD-CORRESPONDING ASSIGN AT AUTHORITY-CHECK
syn keyword abapStatement BACK BREAK-POINT
syn keyword abapStatement CALL CASE CHECK CLEAR CLOSE CNT COLLECT COMMIT COMMUNICATION COMPUTE CONCATENATE CONDENSE CONSTANTS CONTINUE CONTROLS CONVERT CREATE CURRENCY
syn keyword abapStatement DATA DEFINE DELETE DESCRIBE DETAIL DIVIDE DIVIDE-CORRESPONDING DO
syn keyword abapStatement EDITOR-CALL ELSE ELSEIF END-OF-DEFINITION END-OF-PAGE END-OF-SELECTION ENDAT ENDCASE ENDDO ENDEXEC ENDFORM ENDFUNCTION ENDIF ENDIFEND ENDLOOP ENDMODULE ENDON ENDPROVIDE ENDSELECT ENDWHILE EXEC EXIT EXPORT EXPORTING EXTRACT
syn keyword abapStatement EXIT FROM STEP LOOP
syn keyword abapStatement FETCH FIELD-GROUPS FIELD-SYMBOLS FIELDS FORM FORMAT FREE FUNCTION FUNCTION-POOL
syn keyword abapStatement GENERATE GET
syn keyword abapStatement HIDE
syn keyword abapStatement IF IMPORT IMPORTING INFOTYPES INITIALIZATION INPUT INSERT
syn keyword abapStatement LEAVE LIKE LOAD LOCAL LOOP
syn keyword abapStatement MESSAGE MODIFY MODULE MOVE MOVE-CORRESPONDING MULTIPLY MULTIPLY-CORRESPONDING
syn keyword abapStatement NEW-LINE NEW-PAGE NEW-SECTION
syn keyword abapStatement ON OVERLAY
syn keyword abapStatement PACK PARAMETERS PERFORM POSITION PRINT-CONTROL PROGRAM PROVIDE PUT
syn keyword abapStatement RAISE RANGES READ RECEIVE REFRESH REJECT REPLACE REPORT RESERVE RESTORE ROLLBACK
syn keyword abapStatement SCAN SCROLL SEARCH SELECT SELECT-OPTIONS SELECTION-SCREEN SET SHIFT SKIP SORT SPLIT START-OF-SELECTION STATICS STOP SUBMIT SUBTRACT SUBTRACT-CORRESPONDING SUM SUMMARY SUPPRESS SYNTAX-CHECK SYNTAX-TRACE
syn keyword abapStatement TABLES TOP-OF-PAGE TRANSFER TRANSLATE TYPE TYPE-POOL TYPE-POOLS TYPES
syn keyword abapStatement ULINE UNPACK UPDATE
syn keyword abapStatement WHEN WHILE WINDOW WRITE 
" More statemets
syn keyword abapStatement BEGIN END OCCURS STRUCTURE STRUCTURE
syn keyword abapStatement WITH HEADER LINE CASTING APPEND RAISING

" Special ABAP specific tables:
syn match   abapSpecial  "\Wsy-\w\+"ms=s+1
syn match   abapSpecial  "\W\(p\|pa\)\d\d\d\d\W"ms=s+1,me=e-1
syn match   abapSpecial  "\W\(p\|pa\)\d\d\d\d-\w\+"ms=s+1
syn match   abapSpecial  "\Wt\d\d\dt-\w\+"ms=s+1
syn match   abapSpecial  "\Winnnn\W"ms=s+1,me=e-1
syn match   abapSpecial  "\Winnnn-\w\+"ms=s+1
syn keyword abapSpecial  true false null

" Includes
syn region abapInclude   start="include" end="." contains=abapComment 

" Types
syn keyword abapTypes    c n i p f d t x

" Atritmitic operators
syn keyword abapOperator abs sign ceil floor trunc frac acos asin atan cos sin tan
syn keyword abapOperator cosh sinh tanh exp log log10 sqrt

" String operators
syn keyword abapOperator strlen xstrlen charlen numofchar dbmaxlen

" Table operators
syn keyword abapOperator lines

" Table operators (SELECT operators)
syn keyword abapOperator INTO FROM WHERE GROUP BY HAVING ORDER BY
syn keyword abapOperator APPENDING CORRESPONDING FIELDS OF TABLE 
syn keyword abapOperator LEFT RIGHT OUTER INNER JOIN AS CLIENT SPECIFIED BYPASSING BUFFER UP TO ROWS CONNECTING
syn keyword abapOperator EQ NE LT LE GT GE NOT AND OR IN LIKE BETWEEN

" An error? Not strictly... but cannot think of reason this is intended.
syn match   abapError    "\.\."

" Comments
syn region  abapComment  start="^\*" end="$" contains=abapTodo
syn match   abapComment  "\".*" contains=abapTodo
syn keyword abapTodo     contained	TODO NOTE

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_abap_syntax_inits")
  if version < 508
    let did_abap_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink abapError          Error
  HiLink abapComment	      Comment
  HiLink abapInclude	      Include
  HiLink abapSpecial        Special
  HiLink abapSymbolOperator	abapOperator
  HiLink abapOperator	      Operator
  HiLink abapStatement	    Statement
  HiLink abapString	        String
  HiLink abapFloat	        Float
  HiLink abapNumber	        Number
  HiLink abapHex	          Number

  delcommand HiLink
endif

let b:current_syntax = "abap"

" vim: ts=2 sw=2
