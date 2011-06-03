" Vim syntax file
" Language: Lissard
" Maintainer: Jarkko Piiroinen <jarkkop@iki.fi>

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" Source the html syntax file
ru! syntax/html.vim

" Set the filetype to html to load the html ftplugins
set ft=html
unlet b:current_syntax

" Put the Python syntax file in @pythonTop
syn include @pythonTop syntax/python.vim

" End keyword
syn keyword lissardEnd contained end

syn region lissardBlock matchgroup=lissardDelim start=#<%[=|!]\?# end=#%># keepend containedin=ALL contains=@pythonTop,lissardEnd

hi link lissardEnd keyword
hi link lissardDelim delimiter

let b:current_syntax = "lissard"
