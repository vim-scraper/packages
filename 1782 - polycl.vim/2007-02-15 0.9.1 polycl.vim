" Vim syntax file
" Language:     Polyhedra CL
" Maintainer:   Olivier Mengué <dolmen@users.sourceforge.net>
" Last Change:  16 Feb 2006
" License:      Same as Vim
" URL:          http://www.vim.org/scripts/script.php?script_id=1782

" CL is the programming language of stored procedures embedded in the
" Polyhedra database. http://www.enea.com/


if exists("b:current_syntax")
   finish
endif

syn clear

syn keyword polyclTodo contained    TODO FIXME
syn cluster polyclCommentGroup contains=polyclTodo
syn region  polyclCommentL matchgroup=polyclCommentStart start="--" end="$" keepend contains=@polyclCommentGroup
syn region  polyclComment  matchgroup=polyclCommentStart start="/\*" end="\*/"      contains=@polyclCommentGroup

syn match  polyclSpecialError display contained +\\\(u.\{0,4}\|.\|$\)+
syn match  polyclSpecial      display contained +\\\(u\x\{4}\|[ntbrf'"0]\)+
syn region polyclString	start=+"+ skip=+\\\\\|\\"+ keepend excludenl end=+"+re=s-1 end=+$+ contains=polyclSpecialError,polyclSpecial,@Spell

" TODO Fix folding
syn region polyclCustomHandler matchgroup=polyclHandlerStart start="^\s*on\s\+\z(set\s\)\=\ze\s*\z(\w\+\)" end="\<end\s\+\z1\ze\s*\z2\>" transparent fold keepend
syn region polyclHandler matchgroup=polyclHandlerStart start="^\s*on\s\+\z(activate\|create\|delete\|report\)\>" end="\<end\s\+\z1\>" transparent fold keepend
" TODO Fix polyclScript and polyclFunction
syn region polyclScript matchgroup=polyclScriptStart start="^\s*script\>" end="\<end\s\=script\>" transparent fold
syn region polyclFunction matchgroup=polyclFunctionStart start="^\s*function\ze\s\+\w\+\s\+\z(\w\+\)" end="\<end\s\+\z1\>"he=s+3 transparent fold keepend



syn keyword polyclKeyword abort add array as assert at beep by case catch child class commit
syn keyword polyclKeyword component constant create debug default delay delete displayed divide do
syn keyword polyclKeyword else emit end error exit export false fill for foreach forever forget
syn keyword polyclKeyword from function get global go if insert into library link local
syn keyword polyclKeyword locate me multiply new next not null on private public pure
syn keyword polyclKeyword quit reference remove repeat return rollback root schema script send set shared
syn keyword polyclKeyword sleep sql start step stop subtract switch target threadpriority then throw to
syn keyword polyclKeyword transaction true try unlink until update uses while with

syn keyword polyclType    string integer char varchar boolean

syn keyword polyclOperator is old exists is in div not mod and or of
syn keyword polyclOperator char word item line object
" TODO fix this
" syn match   polyclOperator display "\<\%([+-*/^]\|&&\=\|\[<>]=\=\)\>"
" syn match   polyclOperator display "\[+-*/^]\|\[<>]=\="
syn match   polyclOperator display "\<[+-/^=]\>"

syn keyword polyclBuiltin abs acos asin atan atan2 average cos cosh exp max
syn keyword polyclBuiltin min notANumber round sin sinh sqrt sum tan tanh
syn keyword polyclBuiltin truncate
" TODO all CL builtin functions


syn keyword polyclRtrdbFunction getAttribute getCurrentTransactionNo getFirstObject getFTMode
syn keyword polyclRtrdbFunction getNextObject getObjectByInteger getObjectByString getTable
syn keyword polyclRtrdbFunction getWrittenTransactionNo isStandbyActivation


" syn sync ccomment polyclComment minlines=50
syn sync fromstart

set foldmethod=syntax
set foldnestmax=2

hi def link        polyclError          Error
hi def link        polyclSpecialError   Error
hi def link        polyclCommentL       polyclComment
hi def link        polyclCommentStart   polyclComment
hi def link        polyclComment        Comment
hi def link        polyclString         String
hi def link        polyclSpecial        Special
hi def link        polyclTodo           Todo
hi def link        polyclKeyword        Keyword
hi def link        polyclOperator       Operator
hi def link        polyclType           Type
hi def link        polyclHandlerStart   Preproc
hi def link        polyclScriptStart    Preproc
hi def link        polyclFunctionStart  Preproc

hi def link        polyclRtrdbFunction  Keyword


let b:current_syntax = expand('<sfile>:t:r')

" vim: set ts=8:
