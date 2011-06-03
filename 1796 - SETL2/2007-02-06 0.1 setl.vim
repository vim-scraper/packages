" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Assuming case sensitivity
syn case match

syn match setlIdentifier /\(\(program\|end\)[ ]\+\)\@<!\<[A-Za-z]\+[0-9A-Za-z_]*\>/

syn keyword setlConditional end for while loop if then in

syn keyword setlKeywords program exit when

syn keyword setlBoolean true false

syn match setlOperators "\.\.\|#\||\|>\|<\|:"
syn match setlOperators " \(+\|-\|/\|*\|:=\|+:=/\|-:=\|**\) "
syn keyword setlOperators mod max min with fromb frome less from mod npow 
syn keyword setlOperators arb pow forall exists

syn match setlProcedures /\(even\|odd\|char\|float\|type\|str\)\((.\+)\)\@=/
syn match setlProcedures /\(ceil\|floor\|fix\|abs\|log\|exp\)\((.\+)\)\@=/
syn match setlProcedures /\(sin\|cos\|tan\|acos\|asin\|atan\)\((.\+)\)\@=/
syn match setlProcedures /\(atan2\|abs\)\(([^)]\+)\)\@=/
syn match setlProcedures /\(\|print\|read\|get\|printa\|reada\|geta\|eof\|\)\((.\+)\)\@=/
syn match setlProcedures /is_\(float\|fix\|string\)\((.\+)\)\@=/

syn match setlComment /--.*/ contains=@Spell

syn match setlInteger /\<[0-9]\+\>/
syn match setlFloat /[0-9]\+\.[0-9]\+/
syn match setlReal /[0-9]\+\.[0-9]\+E+[0-9]\+/

syn region setlList start=/\[/ end=/\]/ contains=setlConditional,setlOperators,setlKeywords,setlProcedures,setlInteger,setlFloat,setlReal,setlBoolean,setlList,setlTuple,setlIdentifier
syn region setlTuple start=/{/ end=/}/ contains=setlConditional,setlOperators,setlKeywords,setlProcedures,setlInteger,setlFloat,setlReal,setlBoolean,setlList,setlTuple,setlIdentifier

hi link setlConditional Conditional
hi link setlOperators Operator
hi link setlComment Comment 
hi link setlKeywords Keyword
hi link setlProcedures Function
hi link setlInteger Number
hi link setlFloat Number
hi link setlReal Number
hi link setlBoolean Boolean
hi link setlList Special
hi link setlTuple Special
hi link setlIdentifier Identifier
