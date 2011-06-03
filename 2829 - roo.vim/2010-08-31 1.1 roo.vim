" Vim syntax file
" Language:	Spring ROO Command File (http://www.springsource.org/roo)
" Maintainer:	Joris Kuipers (joriskuipers@gmail.com)
" Last Change:	August 31 2010
" Simple syntax file, doesn't try to list the actual Roo commands as these
" will expand with each new Roo version but uses generic syntax instead.

" =============================================================================

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match   rooCommand          "^[a-z ]*\(\n\|\s\)"
syn match   rooLineComment      "^\(//\|;\|#\).*" contains=rooTodo,@Spell
syn region  rooComment          start="/\*" end="\*/" contains=rooTodo,@Spell
syn keyword rooTodo             contained TODO FIXME XXX
syn region  rooPlaceholder      matchgroup=Delimiter start="\${" end="}"
syn match   rooParam            "--\w\+"
syn match   rooTopLevelPackage  "\~"

" Default highlighting
if version >= 508 || !exists("did_roo_syntax_inits")
  if version < 508
    let did_roo_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink rooCommand         Statement
  HiLink rooTodo            Todo
  HiLink rooLineComment     Comment
  HiLink rooComment         Comment
  HiLink rooPlaceholder     Identifier
  HiLink rooParam           PreProc
  HiLink rooTopLevelPackage Special

  delcommand HiLink
endif

let b:current_syntax = "roo"
