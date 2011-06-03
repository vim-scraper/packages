" Vim syntax file
" Language:     WML - Wireless Markup Language
" Maintainer:   Paulo Madeira <acelent @t hotmail d.t com>
" Filenames:    *.wml
" URL:          http://mega.ist.utl.pt/~pbpm/vim/
" Last Change:  13 May 2003

" I didn't find any syntax file for wml, so I just made a simple one

" NOTE This file stands for WML 1.1
" If you want me to ever read your message please start the subject
" with "wml.vim". Ex: "wml.vim sucks!" still has very few changes of
" ever being read. I think you got the idea.

" If you are looking for the "Website META Language" syntax file,
" please take a look at the wml.vim file maintained by Craig Small:
" http://alfie.ist.org/software/vim/syntax/wml.vim

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'wml'
endif

runtime syntax/xml.vim
unlet b:current_syntax

syn keyword wmlTagName contained wml card do onevent head template access meta
syn keyword wmlTagName contained go prev refresh noop postfield setvar select
syn keyword wmlTagName contained optgroup option input fieldset timer img
syn keyword wmlTagName contained anchor a table tr td em strong b i u big small
syn keyword wmlTagName contained p br
syn cluster xmlTagHook add=wmlTagName

syn keyword wmlAttrib contained id class onenterforward onenterbackward ontimer
syn keyword wmlAttrib contained title newcontext ordered type label name
syn keyword wmlAttrib contained optional domain path http-equiv forua content
syn keyword wmlAttrib contained scheme href sendreferer method accept-charset
syn keyword wmlAttrib contained value iname ivalue multiple tabindex onpick
syn keyword wmlAttrib contained format emptyok size maxlength alt src localsrc
syn keyword wmlAttrib contained vspace hspace align height width columns mode
syn cluster xmlAttribHook add=wmlAttrib

hi def link wmlTagName Statement
hi! def link xmlAttrib NONE 
hi def link wmlAttrib Type

let b:current_syntax = "wml"
