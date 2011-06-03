" Vim syntax file
" Language:     Polyhedra CL
" Maintainer:   Olivier Mengué <dolmen@users.sourceforge.net>
" Last Change:  25 Jan 2006
" License:      Same as Vim

" CL is the programming language of stored procudures embedded in the
" Polyhedra database. http://www.enea.com/


if exists("b:current_syntax")
   finish
endif

syn clear

syn keyword polyclTodo contained    TODO FIXME
syn cluster polyCommentGroup contains=polyclTodo
syn region polyclCommentL matchgroup=polyclCommentStart start="--" end="$" keepend contains=@polyclCommentGroup
syn region polyclComment  matchgroup=polyclCommentStart start="/\*" end="\*/" contains=@polyclCommentGroup

syn match  polyclSpecial  display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syn region polyclString	start=+"+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end=+$+ contains=polyclSpecial,@Spell

" TODO Fix folding
syn region polyclCustomHandler matchgroup=polyclHandlerStart start="^\s*on\s\+\z(set\s\)\=\ze\s*\z(\w\+\)" end="\<end\s\+\z1\ze\s*\z2\>" transparent fold keepend
syn region polyclHandler matchgroup=polyclHandlerStart start="^\s*on\s\+\z(activate\|create\|delete\|report\)\>" end="\<end\s\+\z1\>" transparent fold keepend
" TODO Fix polyclScript and polyclFunction
syn region polyclScript matchgroup=polyclScriptStart start="^\s*script\>" end="^\s*end\s\+script\>" transparent fold
syn region polyclFunction matchgroup=polyclFunctionStart start="^\s*function\ze\s\+\w\+\s\+\z(\w\+\)" end="^\s*end\s\+\z1\>" transparent fold keepend



syn keyword polyclKeyword abort add and array as assert at beep by case catch char child class commit
syn keyword polyclKeyword component constant create debug default delay delete displayed div divide do
syn keyword polyclKeyword else emit end error exists exit export false fill for foreach forever forget
syn keyword polyclKeyword from function get global go if in insert into is item library line link local
syn keyword polyclKeyword locate me mod multiply new next not null object of on or private public pure
syn keyword polyclKeyword quit reference remove repeat return rollback root schema script send set shared
syn keyword polyclKeyword sleep sql start step stop subtract switch target threadpriority then throw to
syn keyword polyclKeyword transaction true try unlink until update uses while with word
syn keyword polyclType    string integer char varchar boolean

syn keyword polyclRtrdbFunction getAttribute getCurrentTransactionNo getFirstObject getFTMode
syn keyword polyclRtrdbFunction getNextObject getObjectByInteger getObjectByString getTable
syn keyword polyclRtrdbFunction getWrittenTransactionNo isStandbyActivation


" syn sync ccomment polyclComment minlines=50
syn sync fromstart

set foldmethod=syntax
set foldnestmax=2

hi def link        polyclCommentL polyclComment
hi def link        polyclCommentStart polyclComment
hi def link        polyclComment Comment
hi def link        polyclString  String
hi def link        polyclSpecial Special
hi def link        polyclTodo    Todo
hi def link        polyclKeyword Keyword
hi def link        polyclType    Type
hi def link        polyclHandlerStart   Preproc
hi def link        polyclScriptStart    Preproc
hi def link        polyclFunctionStart  Preproc

hi def link        polyclRtrdbFunction  Keyword


let b:current_syntax = expand('<sfile>:t:r')

" vim: ts=8
