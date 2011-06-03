" Vim syntax file
" Language:     Supercat
" Maintainer:   Gnosis Ouroubouros <vim.20.noos@spamgourmet.com>
" Version Info: 1.1
" Last Change:  2009-06-14

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Clusters
syn cluster supercatAttributes contains=supercatBlink,supercatBold,supercatNormal,supercatReverse,supercatUnderline
syn cluster supercatColors     contains=supercatBlk,supercatBlu,supercatCya,supercatGrn,supercatMag,supercatRed,supercatWhi,supercatYel
syn cluster supercatHtmlColors contains=supercatHtmlBlack,supercatHtmlBlue,supercatHtmlBrown,supercatHtmlCyan,supercatHtmlGreen,supercatHtmlMagenta,supercatHtmlRed,supercatHtmlWhite,supercatHtmlYellow
syn cluster supercatRegexes    contains=supercatRegexBackslash,supercatRegexCarat,supercatRegexClass,supercatRegexCurly,supercatRegexDollar,supercatRegexDot,supercatRegexGroup,supercatRegexOr,supercatRegexQuestion,supercatRegexStar
syn cluster supercatTypes      contains=supercatCharsType,supercatRegexType,supercatStringType,supercatTimeType

" HtmlColors
syn match supercatHtmlBlack   /^Black/     contained
syn match supercatHtmlBlue    /^Blue/      contained
syn match supercatHtmlBrown   /^Brown/     contained
syn match supercatHtmlCyan    /^Cyan/      contained
syn match supercatHtmlGreen   /^Green/     contained
syn match supercatHtmlMagenta /^Magenta/   contained
syn match supercatHtmlRed     /^Red/       contained
syn match supercatHtmlWhite   /^White/     contained
syn match supercatHtmlYellow  /^Yellow/    contained

" Colors
syn match supercatBlk         "^.\{21}blk"hs=e-2,he=e contained contains=@supercatHtmlColors
syn match supercatBlu         "^.\{21}blu"hs=e-2,he=e contained contains=@supercatHtmlColors
syn match supercatCya         "^.\{21}cya"hs=e-2,he=e contained contains=@supercatHtmlColors
syn match supercatGrn         "^.\{21}grn"hs=e-2,he=e contained contains=@supercatHtmlColors
syn match supercatMag         "^.\{21}mag"hs=e-2,he=e contained contains=@supercatHtmlColors
syn match supercatRed         "^.\{21}red"hs=e-2,he=e contained contains=@supercatHtmlColors
syn match supercatWhi         "^.\{21}whi"hs=e-2,he=e contained contains=@supercatHtmlColors
syn match supercatYel         "^.\{21}yel"hs=e-2,he=e contained contains=@supercatHtmlColors

" Attributes
syn match supercatBlink       "^.\{25}k"hs=e,he=e     contained contains=@supercatColors,@supercatHtmlColors
syn match supercatBold        "^.\{25}b"hs=e,he=e     contained contains=@supercatColors,@supercatHtmlColors
syn match supercatNormal      "^.\{25}-"hs=e,he=e     contained contains=@supercatColors,@supercatHtmlColors
syn match supercatReverse     "^.\{25}r"hs=e,he=e     contained contains=@supercatColors,@supercatHtmlColors
syn match supercatUnderline   "^.\{25}u"hs=e,he=e     contained contains=@supercatColors,@supercatHtmlColors

" Number of matches
syn match supercatNumMatches  "^.\{27}\d"hs=e,he=e  contained contains=@supercatAttributes,@supercatColors,@supercatHtmlColors

" Types
syn match supercatCharsType   "^.\{29}c"hs=e,he=e   contained contains=supercatNumMatches,@supercatAttributes,@supercatColors,@supercatHtmlColors
syn match supercatRegexType   "^.\{29}r"hs=e,he=e   contained contains=supercatNumMatches,@supercatAttributes,@supercatColors,@supercatHtmlColors
syn match supercatStringType  "^.\{29}s"hs=e,he=e   contained contains=supercatNumMatches,@supercatAttributes,@supercatColors,@supercatHtmlColors
syn match supercatTimeType    "^.\{29}t"hs=e,he=e   contained contains=supercatNumMatches,@supercatAttributes,@supercatColors,@supercatHtmlColors

" Regexes
syn match   supercatRegexBackslash "\\"    contained
syn match   supercatRegexCarat     "\^"    contained
syn match   supercatRegexDot       "\."    contained
syn match   supercatRegexDollar    "\$"    contained
syn match   supercatRegexOr        "|"     contained
syn match   supercatRegexQuestion  "?"     contained
syn match   supercatRegexStar      "\*"    contained

syn region  supercatRegexCurly     start="{"  skip="\\{"  end="}"  contained oneline
syn region  supercatRegexClass     start="\[" skip="\\\]" end="\]" contained contains=@supercatRegexes oneline
syn region  supercatRegexGroup     start="("  skip="\\)"  end=")"  contained contains=@supercatRegexes oneline

syn match supercatRegex       "^.\{31}.*"hs=s+31 contains=@supercatHtmlColors,@supercatColors,@supercatAttributes,supercatNumMatches,@supercatRegexes,@supercatTypes

" Comments
syn match supercatComment     "^\s*#.*"

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet

highlight def link supercatBlink          Statement
highlight def link supercatBlk            Constant
highlight def link supercatBlu            Constant
highlight def link supercatBold           Statement
highlight def link supercatCharsType      Type
highlight def link supercatComment        Comment
highlight def link supercatCya            Constant
highlight def link supercatGrn            Constant
highlight def link supercatHtmlBlack      Define
highlight def link supercatHtmlBlue       Define
highlight def link supercatHtmlBrown      Define
highlight def link supercatHtmlCyan       Define
highlight def link supercatHtmlGreen      Define
highlight def link supercatHtmlMagenta    Define
highlight def link supercatHtmlRed        Define
highlight def link supercatHtmlWhite      Define
highlight def link supercatHtmlYellow     Define
highlight def link supercatMag            Constant
highlight def link supercatNormal         Statement
highlight def link supercatNumMatches     Number
highlight def link supercatRed            Constant
highlight def link supercatRegex          Special
highlight def link supercatRegexBackslash Typedef
highlight def link supercatRegexCarat     SpecialChar
highlight def link supercatRegexClass     String
highlight def link supercatRegexCurly     Keyword
highlight def link supercatRegexDot       Macro
highlight def link supercatRegexDollar    Exception
highlight def link supercatRegexGroup     Structure
highlight def link supercatRegexOr        Operator
highlight def link supercatRegexQuestion  Conditional
highlight def link supercatRegexStar      Operator
highlight def link supercatRegexType      Type
highlight def link supercatReverse        Statement
highlight def link supercatStringType     Type
highlight def link supercatTimeType       Type
highlight def link supercatUnderline      Underlined
highlight def link supercatWhi            Constant
highlight def link supercatYel            Constant

let b:current_syntax = "supercat"
