" Vim syntax file
" Language:     wiki
" Maintainer:   Andreas Kneib <aporia@web.de>
" Last Change:  2003 Aug 01

" Little syntax file to use a wiki-editor with VIM
" (if your browser allow this action) 
" To use this syntax file:
" 1. mkdir ~/.vim/syntax
" 2. mv ~/wiki.vim ~/.vim/syntax/wiki.vim
" 3. :set syntax=wiki 
"

" Quit if syntax file is already loaded
if exists("b:current_syntax")
        finish
endif

syn match   wikiWord        "\<[A-Z][^A-Z ]\+[A-Z][^A-Z ][^A-Z ]*\>"
syn match   wikiLine        "^----$"
syn region  wikiLink        start="\[" end="\]"
syn match   wikiStar        "[*]"
syn region  wikiCurly       start="{\{3\}" end="}\{3\}"
syn region  wikiHead        start="^= " end="[=] *"
syn region  wikiSubhead     start="^== " end="==[ ]*"
syn match   wikiCurlyError  "}"

hi def link wikiCurlyError  Error
hi def link wikiHead        Type
hi def link wikiSubhead     PreProc
hi def link wikiCurly       Statement
hi def link wikiStar        String
hi def link wikiLink        Comment
hi def link wikiLine        PreProc
hi def link wikiWord        Comment

  
let b:current_syntax = "wiki"

"EOF vim: tw=78:ft=vim:ts=8
