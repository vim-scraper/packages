" Vim syntax file
" Language:     Test Real Time PTS scripts for C testing
" Maintainer:   Samuel Hangouet, adapted from PTU syntax file, by Apurva Shukla <apurva.shukla@gmail.com>
" Last Change:  2005 May 02
" Version:      1.1
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore " The TRT Script language is case insensitive

syn include @CSyntax <sfile>:p:h/c.vim

" Numbers: Warning, uppercase X is not tolerate !
syn match   trtNumber    /\<[8]x[0-9A-F]\+\>/

" Parent Error
syn region  trtParens     start=/(/ end=/)/ contains=trtParens
syn match   trtError      display ")\|}"

syn region  trtNativeDecl start=/#/ end=/$/ keepend contains=trtComment,@CSyntax
syn region  trtNativeCode start=/@/ end=/$/ contains=trtComment,@CSyntax
syn region  trtComment    start=/--/ end=/$/ contains=trtString,trtKeyword,trtStruct,trtNumber
syn region  trtString     start=/"/ skip=/\\"/ end=/"/
syn region  trtBlocAssign start=/{/ end=/}/ contains=trtBlocAssign,@CSyntax

" A bunch of C Test Script Language Keywords
syn keyword trtKeyword SCENARIO END PROC CASE IF THEN ELSE WHILE IS CHANNEL TIMER FORMAT CALLBACK ON LOOP INITIALIZATION TERMINATION EXCEPTION
syn keyword trtStruct  MESSAGE WAITTIL MATCHED MATCHING NOTMATCHING WTIME CALL INCLUDE VAR INIT EV COMMENT FORMAT DEF_MESSAGE OTHERS WHEN ERROR PAUSE EXIT

" Common errors
syn match   trtError /SCN/

" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508
  if version < 508
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
 
" Define the default highlighting.
  HiLink trtKeyword     ModeMsg
  HiLink trtStruct      Statement
  HiLink trtComment     Comment
  HiLink trtNumber      Number
  HiLink trtString      String
  HiLink trtError       Error

  delcommand HiLink
endif

let b:current_syntax = "trt"
