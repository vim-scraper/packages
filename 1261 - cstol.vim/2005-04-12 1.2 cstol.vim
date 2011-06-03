" Language:	OASIS CSTOL files
" Maintainer:	Monique Mudama <mo.mudd@bounceswoosh.org>
" Last Change:	$Date: 2005/04/13 05:03:50 $

syn clear

syn case ignore

syn match cstolLabel "\w\+:\s*$"

syn match cstolCommand "\scheck\s"
syn match cstolCommand "^check\s"
syn match cstolCommand "\slet\s"
syn match cstolCommand "^let\s"
syn match cstolCommand "\sset\s"
syn match cstolCommand "^set\s"
syn match cstolCommand "\swait\s*"
syn match cstolCommand "^wait\s*"
syn match cstolCommand "\scmd\s"
syn match cstolCommand "^cmd\s"
syn match cstolCommand "\swrite\s"
syn match cstolCommand "^write\s"
syn match cstolCommand "\sproc\s"
syn match cstolCommand "^proc\s"
syn match cstolCommand "\send\s\+proc\s*"
syn match cstolCommand "^end\s\+proc\s*"
syn match cstolCommand "\sbegin\s*"
syn match cstolCommand "^begin\s*"
syn match cstolCommand "\sask\s"
syn match cstolCommand "^ask\s"
syn match cstolCommand "\sdeclare\s\+\w*\s"
syn match cstolCommand "^declare\s\+\w*\s"

syn match cstolConditional "\s*if\s"
syn match cstolConditional "\s*else\s*"
syn match cstolConditional "\s*end\s*if\s*"

syn match cstolVariable "\$\w\+"

syn match cstolOperator "\svs\s"
syn match cstolOperator "\swith\s"

syn match cstolNumber "-\=\<\d*\.\=[0-9_]\>"
syn match cstolNumber "\d\+e"

syn match cstolBoolean "true"
syn match cstolBoolean "false"

syn match cstolString "\".*\""

syn match cstolComment ";.*$"

hi link cstolLabel Label
hi link cstolCommand Statement
hi link cstolVariable Identifier
hi link cstolString String
hi link cstolConditional Conditional
hi link cstolOperator Operator
hi link cstolNumber Number
hi link cstolFloat Float
hi link cstolBoolean Boolean
hi link cstolComment Comment

let b:current_syntax = "cstol"
