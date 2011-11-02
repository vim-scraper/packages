" Vim syntax file
" Language:     VDM++
" Maintainer:   Rui Carlos A. Goncalves <rcgoncalves.pt@gmail.com>
" Last Change:  July 20, 2007
"
" Version:      1.0
" Url:          http://rcgoncalves.net/files/vpp.zip

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

" This language is case sensitive
syn case match

" Keywords
syn keyword     vppPart         types functions operations values from
syn keyword     vppType         bool int nat nat1 real rat char token
syn keyword     vppBool         true false
syn keyword     vppConstant     nil
syn keyword     vppConditional  if then else elseif
syn keyword     vppStatement    let def be st
syn keyword     vppStatement    cases others
syn keyword     vppStatement    pre post inv 
syn keyword     vppStatement    compose
syn keyword     vppStatement    forall exists exists1 iota
syn keyword     vppStatement    lambda
syn keyword     vppStatement    undefined skip
syn keyword     vppStatement    dcl
syn keyword     vppStatement    ext rd wr errs
syn keyword     vppStatement    while do by reverse
syn keyword     vppStatement    return
syn keyword     vppStatement    always trap with tixe exit
syn keyword     vppStatement    error
syn keyword     vppStatement    static public private protected new
syn keyword     vppStatement    sync per thread periodic threadid
syn keyword     vppStatement    self
syn keyword     vppStatement    atomic
syn keyword     vppFunction     mu
syn keyword     vppFunction     not and or
syn keyword     vppFunction     abs floor div mod rem
syn keyword     vppFunction     union inter subset psubset card dunion dinter power
syn keyword     vppFunction     hd tl len elems inds conc
syn keyword     vppFunction     dom rng munion merge comp inverse
syn keyword     vppFunction     isofbaseclass isofclass samebaseclass sameclass
syn keyword     vppFunction     start startlist

" Delimiters
syn match       vppDelimiter    "(\|)\|\[\|\]\|{\|}\|,"

" Operators
syn match       vppOperator     "\.\|:\|::\|->\|+>\|==\|=>\|<=>\|=\|<>\|+\|-\|*\|/\|*\*\|<\|>\|<=\|>=\|&\||\|\^\|\\\|++\||->\|<:\|<-:\|:>\|:->\|.#\|:-\|==>\|:=\|||\|:-\|\*\*"

" Strings and constants
syn match       vppNumber       "\<[0-9]\+\>"
syn match       vppFloat        "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"
syn match       vppConstant     "<[a-zA-Z][a-zA-Z0-9_']*>"
syn match       vppSpecialChar  "\\\([rntfea\"\\']\|x[0-9a-fA-F][0-9a-fA-F]\|c.\|[0-7][0-7][0-7]\)" contained
syn match       vppCharacter    "[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=vppSpecialChar
syn match       vppCharacter    "^'\([^\\]\|\\[^']\+\|\\'\)'" contains=vppSpecialChar
syn region      vppString       start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=vppSpecialChar

" Other types
syn match       vppType         "\<\(set\|seq\|seq1\) *of\>"
syn region      vppNormalMap1   matchgroup=vppType start="\<\(map\|inmap\)\>" end="\<to\>" contains=vppType,vppNormalMap2,vppStatement,vppOperator,vppConstant
syn region      vppNormalMap2   matchgroup=vppType start="\<\(map\|inmap\)\>" end="\<to\>" contains=vppType,vppNormalMap1,vppStatement,vppOperator,vppConstant contained

" Other statements
syn match       vppStatement    "\<end\>"
syn match       vppStatement    "\<of\>"
syn match       vppStatement    "\<in\>"
syn match       vppStatement    "\<is *not *yet *specified\>"
syn match       vppStatement    "\<is *subclass *responsibility\>"
syn match       vppStatement    "\<to\>"
syn match       vppStatement    "\<\(for *all\|for\)\>"
syn match       vppStatement    "\<mutex\>"
syn match       vppStatement    "\<match\>"
syn match       vppStatement    "\<all\>"

" Other functions
syn match       vppFunction     "\<in *set\>"
syn match       vppFunction     "\<\(mk_[a-zA-Z][a-zA-Z0-9_'`]*\|mk_\)\>"
syn match       vppFunction     "\<\(is_[a-zA-Z][a-zA-Z0-9_']*\|is_\)\>"
syn match       vppFunction     "\<init_[a-zA-Z][a-zA-Z0-9_']*\>"
syn match       vppFunction     "\<inv_[a-zA-Z][a-zA-Z0-9_']*\>"
syn match       vppFunction     "\<pre_[a-zA-Z][a-zA-Z0-9_']*\>"
syn match       vppFunction     "\<post_[a-zA-Z][a-zA-Z0-9_']*\>"
syn match       vppFunction     "\(#act\|#fin\|#active\|#req\|#waiting\)\>"

" Others
syn match       vppPart         "\<class\>"
syn match       vppPart         "\<instance *variables"
syn match       vppPart         "\<is *subclass *of\>"
syn match       vppPart         "\<end[\ \r\n]*[a-zA-Z][a-zA-Z0-9_']*[\ \r\n]*class\>"he=s+3,me=e-5
syn match       vppPart         "\<end[\ \r\n]*[a-zA-Z][a-zA-Z0-9_']*[\ \r\n]*\%$"he=s+3

" Comments
syn match       vppLineComment  "--.*"


hi def  link    vppOperator     Operator
hi def  link    vppPart         PreProc
hi def  link    vppType         Type
hi def  link    vppStatement    Statement
hi def  link    vppConditional  Conditional
hi def  link    vppLineComment  Comment
hi def  link    vppSpecialChar  SpecialChar
hi def  link    vppString       String
hi def  link    vppCharacter    Character
hi def  link    vppFloat        Float
hi def  link    vppNumber       Number
hi def  link    vppBool         Boolean
hi def  link    vppConstant     Constant
hi def  link    vppFunction     Function
