" Vim syntax file
" Language:	    sablecc (Url: http://sablecc.org/)
" Maintainer:	Hong Phuc Bui (String highlight is modified from Java String of
"               Claudio Fleiner <claudio@fleiner.com> )
" Last Change:	2010.07.15
" Status:       Beta


syn keyword sableccSection   Package Helpers Tokens Productions
syn match   sableccSection   'Ignored\sTokens'
syn match   sableccSection   'Abstract\sSyntax\sTree'

syn match   sableccOperator  '[\|\*\?\+\()=-]'

syn match   sableccCharacter '\'\p\'\|\'\'\''



" Strings and constants
" String
syn match   sableccString        '\'\S\{2,}\''
syn match   sableccStringError   '\'\s\{2,}\S*\''
syn match   sableccProduction    '{\w*}'


" Comments
syn keyword sableccTodo		 contained TODO FIXME XXX NOTE NOTES
syn match   sableccSpecialChar      contained "\\\([4-9]\d\|[0-3]\d\d\|[\"\\'ntbrf]\|u\+\x\{4\}\)"
syn match   sableccSpecial "\\u\+\d\{4\}"
syn match   sableccNumber		 "\<\(0[0-7]*\|0[xX]\x\+\|\d\+\)[lL]\=\>"
syn match   sableccNumber		 "\(\<\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[fFdD]\="
syn match   sableccNumber		 "\<\d\+[eE][-+]\=\d\+[fFdD]\=\>"
syn match   sableccNumber		 "\<\d\+\([eE][-+]\=\d\+\)\=[fFdD]\>"


syn region  sableccCommentString    contained start=+"+ end=+"+ end=+$+ end=+\*/+me=s-1,he=s-1 contains=sableccSpecial,sableccCommentStar,sableccSpecialChar,@Spell
syn region  sableccComment2String   contained start=+"+  end=+$\|"+  contains=sableccSpecial,sableccSpecialChar,@Spell
syn match   sableccCommentCharacter contained "'\\[^']\{1,6\}'" contains=sableccSpecialChar
syn match   sableccCommentCharacter contained "'\\''" contains=sableccSpecialChar
syn match   sableccCommentCharacter contained "'[^\\]'"
syn cluster sableccCommentSpecial add=sableccCommentString,sableccCommentCharacter,sableccNumber
syn cluster sableccCommentSpecial2 add=sableccComment2String,sableccCommentCharacter,sableccNumber

syn region  sableccComment		 start="/\*"  end="\*/" contains=@sableccCommentSpecial,sableccTodo,@Spell
syn match   sableccCommentStar      contained "^\s*\*[^/]"me=e-1
syn match   sableccCommentStar      contained "^\s*\*$"
syn match   sableccLineComment      "//.*" contains=@sableccCommentSpecial2,sableccTodo,@Spell

hi def link sableccComment          Comment

hi def link sableccSection     Special
hi def link sableccOperator    Statement
hi def link sableccCharacter   Type
hi def link sableccProduction  PreProc
hi def link sableccString      String
hi def link sableccStringError Error

hi def link sableccNumber         Number
hi def link sableccTodo           Todo


