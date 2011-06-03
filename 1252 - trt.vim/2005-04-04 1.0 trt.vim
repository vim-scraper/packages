" Vim syntax file
" Language:	Test Real Time PTS scripts
" Maintainer:	Samuel Hangouet, adapted from PTU syntax file, by Apurva Shukla <apurva.shukla@gmail.com>
" Last Change:	2005 Feb 02
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

" A bunch of C Test Script Language Keywords
syn keyword trtKeyword SCENARIO END PROC CASE IF THEN ELSE WHILE IS CHANNEL TIMER FORMAT CALLBACK ON LOOP INITIALIZATION TERMINATION EXCEPTION
syn keyword trtStruct  MESSAGE WAITTIL MATCHED MATCHING NOTMATCHING WTIME CALL INCLUDE VAR INIT EV COMMENT FORMAT DEF_MESSAGE OTHERS WHEN ERROR PAUSE EXIT

" Numbers: Warning, uppercase X is not tolerate !
syn match trtNumber "8x[0-9A-F]\+"
syn match trtNumber "b[01]\+"

syn region trtNativeDecl start=/#/ end=/$/ contains=trtComment, @CSyntax
syn region trtNativeCode start=/@/ end=/$/ contains=trtComment, @CSyntax
" No 'keepend': Strings can flood out of comments and trigger some strange effects !
syn region trtComment	 start="--" end="$" contains=trtString
syn region trtString     start="\"" end="\""

" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508
  if version < 508
    let did_c_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
 
" Define the default highlighting.
  HiLink trtKeyword		ModeMsg
  HiLink trtStruct 		Statement
  HiLink trtComment		Comment
  HiLink trtNumber              Number
  HiLink trtString              String

  delcommand HiLink
endif

let b:current_syntax = "trt"
