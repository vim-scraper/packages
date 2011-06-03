" Vim syntax file
" Language:	MDL
" Maintainer:	Sapan Bhatia <sapan@corewars.org>
" Last Change:	1.4.2003

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif


syn keyword main Module Imports Exports Defines From needs inits
syn keyword types int char long void unsigned contained
syn keyword extintstruct extern intern struct contained
syn match base /[SDZ]([a-zA-Z \*]*)/ contained
syn match defn /[A-Za-z_]*::.*/ contains=extintstruct,types,base
highlight link main Keyword
highlight link types Type
highlight link extintstruct Define
highlight base cterm=bold
