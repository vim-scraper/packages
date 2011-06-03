" based on cmake.vim

syn case ignore
syn region sgfComment start=/C\[/ end=/\]/ skip=/\\[\[\]]/
            \ contained
syn region sgfOptions start=/\[/ end=/\]/ skip=/\\[\[\]]/
            \ contained
syn match sgfArgument /[^()"]+/
            \ contained
syn keyword sgfKeyword
            \ B W LB N
syn keyword sgfMainKeyword
            \ FF GM SZ AP
syn keyword sgfEntryKeyword
            \ PB PW KM DT TM BR WR RE
syn region sgfArguments start=/\s*(/ end=/)/
           \ contains=ALLBUT,sgfArguments

if version >= 508
  command -nargs=+ HiLink hi def link <args>

  HiLink sgfOptions Underlined
  HiLink sgfKeyword Operator
  HiLink sgfMainKeyword String
  HiLink sgfEntryKeyword Type

  HiLink sgfArguments Identifier
  HiLink sgfArgument Constant
  HiLink sgfComment Comment

  delcommand HiLink
endif

let b:current_syntax = "sgf"

