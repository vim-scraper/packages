" Vim syntax file
" Language:     CIM Managed Object Format (MOF)
" Maintainer:   Mike Martin <mmartin5@austin.rr.com>
" URL:          N/A
" Last Change:  13 August 2002

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Keywords
"
syn case ignore
syn keyword mofStructure        class indication instance qualifier of as
syn keyword mofStructure        scope nextgroup=mofScope
syn keyword mofStructure        flavor nextgroup=mofDefaultFlavor
syn keyword mofNull             null
syn keyword mofBoolean          true false
syn keyword mofType             boolean char16 datetime real32 real64 sint16 sint32 sint64 sint8 string uint16 uint32
syn keyword mofType             uint64 uint8 ref
syn keyword mofMetaElement      contained schema class association indication qualifier property reference method
syn keyword mofMetaElement      contained parameter any
syn keyword mofFlavor           contained disableoverride enableoverride restricted tosubclass translatable
" Microsoft-specific flavors
syn keyword mofFlavor           contained amended nottoinstance nottosubclass toinstance
syn case match

" Preprocessor
"
syn match   mofPreProc          "^\s*#\k\+"

" Qualifier declarations
"
syn region  mofScope            start="(" end=")" contained contains=mofMetaElement
syn region  mofDefaultFlavor    start="(" end=")" contained contains=mofFlavor

" Qualifier lists
"
syn region  mofQualifierList    start="\[[^][:digit:]]" end="]" contains=mofQualifierParam,mofFlavor
syn region  mofQualifierParam   start="(" end=")" transparent contained contains=@mofConstant
syn region  mofQualifierParam   start="{" end="}" transparent contained contains=@mofConstant

" Comments
"
syn keyword mofTodo             contained TODO FIXME XXX NOTE
syn region  mofComment          start="/\*" end="\*/" contains=mofTodo
syn match   mofComment          "//.*$" contains=mofTodo

" Strings and other constants
" TODO special highlighting for unicode strings ?
syn match   mofSpecialError     contained "\\."
syn match   mofSpecialCharError contained "[^']"
syn match   mofSpecialChar      contained "\\\([4-9]\d\|[0-3]\d\d\|[\"\\'ntbrf]\|u\x\{4\}\)"
syn region  mofString           start=+"+ end=+"+ end=+$+ contains=mofSpecialChar,mofSpecialError
syn match   mofCharacter        "'[^']*'" contains=mofSpecialChar,mofSpecialCharError
syn match   mofCharacter        "'\\''" contains=mofSpecialChar
syn match   mofCharacter        "'[^\\]'"
syn match   mofNumber           "\<\(0[0-7]*\|0[xX]\x\+\|\d\+\)[lL]\=\>"
syn match   mofFloat            "\(\<\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[fFdD]\="
syn match   mofFloat            "\<\d\+[eE][-+]\=\d\+[fFdD]\=\>"
syn match   mofFloat            "\<\d\+\([eE][-+]\=\d\+\)\=[fFdD]\>"
syn cluster mofConstant         contains=mofNull,mofBoolean,mofString,mofCharacter,mofNumber,mofFloat

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_mof_syn_inits")
  if version < 508
    let did_mof_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink mofStructure        Structure
  HiLink mofNull             Constant
  HiLink mofBoolean          Boolean
  HiLink mofType             Type
  HiLink mofMetaElement      Keyword
  HiLink mofFlavor           Keyword
  HiLink mofPreProc          PreProc
  HiLink mofQualifierList    SpecialComment
  HiLink mofTodo             Todo
  HiLink mofComment          Comment
  HiLink mofSpecialError     Error
  HiLink mofSpecialCharError Error
  HiLink mofSpecialChar      SpecialChar
  HiLink mofString           String
  HiLink mofCharacter        Character
  HiLink mofNumber           Number
  HiLink mofFloat            Float

  delcommand HiLink
endif


let b:current_syntax = "mof"

" vim: ts=8
