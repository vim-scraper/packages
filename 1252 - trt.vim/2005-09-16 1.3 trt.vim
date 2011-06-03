" Vim syntax file
" Language:     TestRealTime scripts for C testing
" Maintainer:   Samuel Hangouet, adapted from PTU syntax file, by Apurva Shukla <apurva.shukla@gmail.com>
" Last Change:  2005 Sep 16
" Version:      1.3

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn include @CSyntax <sfile>:p:h/c.vim
unlet b:current_syntax

" A bunch of C Test Script Language Keywords
syn keyword trtKeyword  THEN ELSE IS CHANNEL TIMER FORMAT CALLBACK ON LOOP INITIALIZATION TERMINATION EXCEPTION
syn match   trtKeyWord  /\<\(END \)\=\(PROC\|IF\|WHILE\|CASE\)\>/
syn keyword trtStruct   MESSAGE WAITTIL MATCHED MATCHING NOTMATCHING WTIME CALL INCLUDE VAR INIT EV COMMENT FORMAT DEF_MESSAGE OTHERS WHEN ERROR PAUSE EXIT
syn match   trtScenario /\<\(END \)\=SCENARIO\>/

" Numbers: Warning, uppercase X is not tolerate !
syn match   trtNumber    /\<[8]x[0-9A-F]\+\>/

" In TRT 2002, comments are parsed, and not closed double-quote can sometimes trigger an error (wonderful soft...)
syn region  trtComment    start=/--/ skip=/\n--/ end=/$/ contains=trtString
syn region  trtString     start=/"/ skip=/\\"/ end=/"/

" C Syntax inside:
syn region  trtNativeDecl start=/#/ skip=/\(\\\n\)\|\(\n[&#]\)/ end=/$/ keepend contains=@CSyntax
syn region  trtNativeCode start=/@/ skip=/\(\\\n\)\|\(\n[&@]\)/ end=/$/ keepend contains=@CSyntax
syn region  trtBlocAssign start=/{/ end=/}/ contains=trtBlocAssign,@CSyntax,trtClosingError

" Closing brackets errors
syn region  trtParens     start=/(/ end=/)/ contains=trtParens,trtStruct,trtClosingError
syn match   trtError             display ")\|}"
syn match   trtClosingError      display "^[^&]" contained

" Common other errors
syn match   trtError /SCN/
syn match   trtError /END\s\+SCENARIO\s\+\w/hs=e

" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508
  if version < 508
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
 
" Define the default highlighting.
  HiLink trtKeyword            Type
  HiLink trtStruct             Statement
  HiLink trtComment            Comment
  HiLink trtNumber             Number
  HiLink trtString             String
  HiLink trtError              Error
  HiLink trtClosingError       Error
  HiLink trtScenario           ModeMsg

  delcommand HiLink
endif

let b:current_syntax = "trt"
