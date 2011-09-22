" Vim syntax file
" Language: Web IDL
" Maintainer: Alexander Futász <aldafu+vim@gmail.com>
" Last Change: 2011 Jun 21
" Remark:

if exists("b:current_syntax")
	finish
endif

syntax case match

highlight def link widlType Type
syntax keyword widlType boolean byte octet float double DOMString unsigned short long Object Date any sequence void 

highlight def link widlKeyword Keyword
syntax keyword widlKeyword module interface exception typedef dictionary 

highlight def link widlConst Constant
syntax keyword widlConst const

highlight def link widlStorage StorageClass
syntax keyword widlStorage in readonly static raises implements

highlight def link widlAttrs Operator
syntax keyword widlAttrs stringifier attribute getraises setraises omittable getter setter creator deleter caller optional

highlight def link widlComment Comment
syntax region widlComment start=:/\*: end=:\*/: contains=widlDocTag

highlight def link widlComment2 Comment
syntax match widlComment2 ://.*:

highlight def link widlDocTag SpecialComment
syntax match widlDoctag :\\api-feature\|\\author\|\\b\(rief\)\?\|\\code\|\\endcode\|\\def-api-feature\|\\def-api-feature-set\|\\def-device-cap\|\\def-instantiated\|\\device-cap\|\\n\|\\param\|\\return\|\\throw\|\\version: contained

highlight def link widlExtAttr Define
syntax match widlExtAttr :\[\(\u\(\a\)\+\(,\)\?.*\)\+\]:

highlight def link widlString Character
syntax region widlString start=:': skip=:\\': end=:':

highlight def link widlString2 Character
syntax region widlString2 start=:": skip=:\\": end=:":

highlight def link widlNum Number
syntax match widlNum :\<\d\+\>:

highlight def link widlHexNum Number
syntax match widlHexNum :\<0[xX]\d\+\>:

highlight def link widlOctalNum Number
syntax match widlOctalNum :\<\o\+\>:

highlight def link widlBool Boolean
syntax keyword widlBool true false
