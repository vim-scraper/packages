" Vim syntax file
" Language:	AMPLE AnotherMentorProgrammingLanguage
" Maintainer:	Alvin Santos <aas_spam@yahoo.com>
" Last change:  2003-Jun-20 12:54:56 AM 
" Extensions:   *.ample *.dofile
" Comment:      AMPLE is an proprietary language created by MGC (a
"		company specializing in Electronics Design Automation), for
"		the purposes of customizing Mentor's Falcon Framework
"		enviroment 
"		Many thanks to the dracula syntax file and the spice syntax file.
"Version:       0.1 - inital release

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Ignore case
syn case ignore

" A bunch of useful Ample keywords

syn keyword ampleKeyword function return if else assignment switch break continue while do for indirect invisible sealed undefined asm auto char class const delete double enum float friend goto inline int long new operator overload public register short signed sizeof struct this typedef union unsigned virtual volatile
syn keyword ampleStorage extern local declaration literal static builtin command usage quick_help ref_help string optional name default check label rest

"Match Stings
"syn match   calibreString /"[^"]*"/hs=s+1,he=e-1
syn match   ampleString /"[^"]*"/
syn match ampleCommand "\$[A-Za-z_]*"
" Numbers, all with engineering suffixes and optional units
"==========================================================
"floating point number, with dot, optional exponent
syn match ampleNumber  "\<[0-9]\+\.[0-9]*\(e[-+]\=[0-9]\+\)\=\(meg\=\|[afpnumkg]\)\="
"floating point number, starting with a dot, optional exponent
syn match ampleNumber  "\.[0-9]\+\(e[-+]\=[0-9]\+\)\=\(meg\=\|[afpnumkg]\)\="
"integer number with optional exponent
syn match ampleNumber  "\<[0-9]\+\(e[-+]\=[0-9]\+\)\=\(meg\=\|[afpnumkg]\)\="

"syn match   calibrePreProc "^#.*"
syn match   amplePreProc "#.*"

syn match   ampleComment "//.*"
syn region  ampleBlockComment start="/\*" end="\*/"
"Modify the following as needed.  The trade-off is performance versus
"functionality.
syn sync lines=50

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_calibre_syn_inits")
  if version < 508
    let did_calibre_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink calibreIdentifier     Identifier
  HiLink calibreStatement      Statement
  HiLink ampleKeyword          Keyword
  HiLink ampleStorage          StorageClass
  HiLink ampleComment          Comment
  HiLink ampleBlockComment     Comment
  HiLink amplePreProc          PreProc
  HiLink ampleString           String
  HiLink ampleNumber           Number
  HiLink calibreRuleName       Statement
  HiLink ampleCommand          Structure

  delcommand HiLink
endif

let b:current_syntax = "ample"

" vim: ts=8
