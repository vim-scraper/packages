" Language: tmda filter file
" Maintainer:   Monique Y. Mudama <mo.mudd@bounceswoosh.org>
" Last Change:  $Date: 2005/04/13 04:43:05 $
" Remark:  I know this is incomplete; please give me feedback!

syn clear

"add dash to the list of acceptable chars in keywords
set iskeyword+=-

syn keyword tfSource headers to from body size pipe
syn keyword tfCommand include
syn keyword tfSource to-file from-file body-file headers-file
syn keyword tfSource to-cdb from-cdb
syn keyword tfSource to-dbm from-dbm
syn keyword tfSource to-ezmlm from-ezmlm
syn keyword tfSource to-mailman from-mailman
syn keyword tfSource to-sql from-sql


syn keyword tfAction ok accept deliver
syn keyword tfAction hold confirm
syn keyword tfAction bounce reject drop exit stop
syn match tfAction "deliver=.*$"
syn match tfAction "ok=.*$"
syn match tfAction "accept=.*$"
syn match tfAction "reject=.*$"
syn match tfAction "bounce=.*$"
syn match tfAction "confirm=.*$"

syn match tfComment "#.*$"

syn region tfString start=+"+ end=+"+ skip=+\\"+
syn region tfString start=+'+ end=+'+ skip=+\\'+

syn match tfEmail "[a-zA-Z0-9\-\.\*\+]\+@=\=[a-zA-Z0-9\-\.\*]\+"

hi link tfSource Statement
hi link tfCommand Statement
hi link tfAction Function
hi link tfComment Comment
hi link tfString String
hi link tfEmail String

let b:current_syntax = "tmda_filter"
