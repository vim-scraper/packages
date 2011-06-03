" Vim syntax file
" Language: ipclog file
" Maintainer: AZhidenkov <bff7755a@yandex.ru>
" Last change: 2011 Mar 03

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax sync fromstart

syn match ipcError "ERROR : .*$"
syn match ipcVar /\$\{1,2\}[A-Za-z_0-9]\+/
syn match ipcDate /\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d/
syn match ipcString /"[^"]\+"/
syn match ipcORA /ORA-\d\d\d\d\d.*$/

hi def link ipcError PreProc
hi def link ipcVar Identifier
hi def link ipcDate Type
hi def link ipcString Comment
hi def link ipcORA ModeMsg

let b:current_syntax = "ipclog"
