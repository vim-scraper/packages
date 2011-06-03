" Vim syntax file
" Language:     Plumbing (for Plan 9's plumbing)
" Maintainer:   Gnosis Ouroubouros <vim.20.noos@spamgourmet.com>
" Version Info: 1.1
" Last Change:  2009-06-04

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn cluster plumbingNormVerb  contains=plumbingAdd,plumbingIs,plumbingIsFile,plumbingMatches,plumbingSet
syn cluster plumbingPlumbVerb contains=plumbingClient,plumbingStart,plumbingTo

syn match plumbingAdd       /\<add\>/               nextgroup=plumbingName       skipwhite
syn match plumbingArg       /^arg\>/                nextgroup=@plumbingNormVerb  skipwhite
syn match plumbingAttr      /^attr\>/               nextgroup=@plumbingNormVerb  skipwhite
syn match plumbingClient    /\<client\>/            nextgroup=plumbingShell      skipwhite
syn match plumbingComment   /^#.*$/
syn match plumbingData      /^data\>/               nextgroup=@plumbingNormVerb  skipwhite
syn match plumbingDst       /^dst\>/                nextgroup=@plumbingNormVerb  skipwhite
syn match plumbingEquals    /=/                     nextgroup=plumbingValue      skipwhite
syn match plumbingFile      /\<.*$/       contained
syn match plumbingInclude   /^include\>/            nextgroup=plumbingFile       skipwhite
syn match plumbingIs        /\<is\>/                nextgroup=plumbingIsArg      skipwhite
syn match plumbingIsArg     /\<.*$/       contained
syn match plumbingIsFile    /\<isfile\>/            nextgroup=plumbingFile       skipwhite
syn match plumbingMatches   /\<matches\>/           nextgroup=plumbingRegex      skipwhite
syn match plumbingName      /\<\w\+\>/    contained nextgroup=plumbingEquals     skipwhite
syn match plumbingPlumb     /^plumb\>/              nextgroup=@plumbingPlumbVerb skipwhite
syn match plumbingPort      /\<[^ ]\+\>/  contained
syn match plumbingRegex     /\<.*$/       contained
syn match plumbingShell     /\<.*$/       contained
syn match plumbingSet       /\<set\>/               nextgroup=plumbingSetArg     skipwhite
syn match plumbingSetArg    /\<.*/        contained contains=plumbingVariable    skipwhite
syn match plumbingStart     /\<start\>/             nextgroup=plumbingShell      skipwhite
syn match plumbingTo        /\<to\>/                nextgroup=plumbingPort       skipwhite
syn match plumbingType      /^type\>/               nextgroup=@plumbingNormVerb  skipwhite
syn match plumbingValue     /\<[^ ]\+\>/  contained
syn match plumbingVariable  /\$\w\+/

syntax region plumbingString start=/'/ end=/'/

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet

highlight def link plumbingArg     plumbingObject
highlight def link plumbingAttr    plumbingObject
highlight def link plumbingComment plumbingObject
highlight def link plumbingData    plumbingObject
highlight def link plumbingDst     plumbingObject
highlight def link plumbingInclude plumbingObject
highlight def link plumbingPlumb   plumbingObject
highlight def link plumbingType    plumbingObject

highlight def link plumbingClient  plumbingVerb
highlight def link plumbingIs      plumbingVerb
highlight def link plumbingIsFile  plumbingVerb
highlight def link plumbingMatches plumbingVerb
highlight def link plumbingSet     plumbingVerb
highlight def link plumbingStart   plumbingVerb
highlight def link plumbingTo      plumbingVerb

highlight def link plumbingAdd     Function
highlight def link plumbingArg     Identifier
highlight def link plumbingAttr    Identifier
highlight def link plumbingClient  Function
highlight def link plumbingComment Comment
highlight def link plumbingData    Identifier
highlight def link plumbingDst     Identifier
highlight def link plumbingEquals  Operator
highlight def link plumbingFile    Constant
highlight def link plumbingInclude Include
highlight def link plumbingIs      Function
highlight def link plumbingIsArg   Constant
highlight def link plumbingIsFile  Function
highlight def link plumbingMatches Function
highlight def link plumbingName    Label
highlight def link plumbingPlumb   Macro
highlight def link plumbingPort    Constant
highlight def link plumbingRegex   Special
highlight def link plumbingShell   Constant
highlight def link plumbingSet     Function
highlight def link plumbingSetArg  Constant
highlight def link plumbingStart   Function
highlight def link plumbingString  String
highlight def link plumbingTo      Function
highlight def link plumbingType    Identifier
highlight def link plumbingValue   Constant

let b:current_syntax = "plumbing"
