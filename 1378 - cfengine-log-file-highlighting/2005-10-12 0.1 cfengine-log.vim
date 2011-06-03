" cfengine log syntax file
" Filename:     cfengine-log.vim
" Language:     cfengine log
" Version:      0.1
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

syn region  cfengineSection contains=cfengineActionType start="^\*\*\*" end="\*\*\*$"

" files,copy section
syn region  cfengineCopy            contains=cfengineMode start="^cfengine:.*Update of image" end="$" oneline 
syn region  cfengineCopy            contains=cfengineMode start="^cfengine:.*Copying from" end="$" oneline 
syn region  cfengineCopy            contains=cfengineMode start="^cfengine:.*Creating file" end="$" oneline 
syn region  cfengineCopy            contains=cfengineMode start="^cfengine:.*Object.*had permission" end="$" oneline

" editfiles section
syn region  cfengineEdit contains=cfengineFilename start="^Begin editing" end="End editing.*$"

" shellcommands section
syn match   cfengineExecuting "^cfengine:.*Executing script.*\.\.\."

" tidy sedction
syn match   cfengineDelete    "^cfengine:.*Deleting.*$"

" general
syn match cfengineMode "mode.*= \d*"
syn match cfengineMode "\d\d\d"
syn match cfengineMode "\d\d\d\d"

syn match cfengineActionType "control.*pass \d"
syn match cfengineActionType "copy.*pass \d"
syn match cfengineActionType "files.*pass \d"
syn match cfengineActionType "editfiles.*pass \d"
syn match cfengineActionType "tidy.*pass \d"
syn match cfengineActionType "directories.*pass \d"
syn match cfengineActionType "groups.*pass \d"
syn match cfengineActionType "classes.*pass \d"
syn match cfengineActionType "processes.*pass \d"
syn match cfengineActionType "shellcommands.*pass \d"
syn match cfengineActionType "strategies.*pass \d"
syn match cfengineActionType "links.*pass \d"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508
  command -nargs=+ HiLink hi def link <args>

  HiLink cfengineSection            Comment
  HiLink cfengineCopy               Type
  HiLink cfenginePermissions        Type
  HiLink cfengineEdit               String
  HiLink cfengineActionType         PreProc
  HiLink cfengineDelete             Type
"  HiLink cfengineBoolean            Boolean
"  HiLink cfengineOption             Statement
  HiLink cfengineExecuting          Type
"  HiLink cfengineTodo               Todo
  HiLink cfengineMode               Number
  HiLink cfengineBeginEndEdit       Identifier

  delcommand HiLink
endif

let b:current_syntax = "cfengine-log"
