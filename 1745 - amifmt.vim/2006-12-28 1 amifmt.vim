" Vim syntax file for AmiFormat
" Language: AmiFormat
" Version: 1
" Last Change: 2006-12-28 Thu
" Maintainer: Swaroop <swaroopNOSPAM@swaroopch.info>
" Reference: http://orangoo.com/labs/AmiNation/AmiFormat/

"""""""""""""""""""" Initial Checks """"""""""""""""""""""""""""""""""""""""""""

" To be compatible with Vim 5.8. See `:help 44.12`
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    " Quit when a (custom) syntax file was already loaded
    finish
endif

"""""""""""""""""""" Patterns """"""""""""""""""""""""""""""""""""""""""""""""""

" Emphasis
syn match amiItalic /<i>.\{-}<\/i>/
syn match amiBold /<b>.\{-}<\/b>/

" Todo
syn keyword amiTodo TODO FIXME XXX

" Headings
syn match amiHeading /^h[1-6]\.\s\+.\{-}$/

" Lists
syn match amiList /^\s*\*\s\+/
syn match amiList /^\s*\d\+\.\s\+/

" Classes
syn match amiClass /^\s*%(\w\+).*%/
syn match amiClass /^\s*%{.*}.*%/

" Code
syn region amiCode excludenl start=/\[code\]/ end=/\[\/code\]/

" HTML
syn region amiEscape excludenl start=/\[escape\]/ end=/\[\/escape\]/

" Link
syn match amiLink /".\{-}":(.\{-})/

" Image
syn match amiImage /!.\{-}(.\{-})!/

"""""""""""""""""""" Highlighting """"""""""""""""""""""""""""""""""""""""""""""

hi def amiItalic term=italic cterm=italic gui=italic
hi def amiBold term=bold cterm=bold gui=bold

hi def link amiHeading Title
hi def link amiTodo Todo
hi def link amiList PreProc
hi def link amiClass Statement
hi def link amiCode Identifier
hi def link amiEscape Comment
hi def link amiLink String
hi def link amiImage String

"""""""""""""""""""" Finish """"""""""""""""""""""""""""""""""""""""""""""""""""

" Set syntax name
let b:current_syntax = "amifmt"

" vim: filetype=vim
