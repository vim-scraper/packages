" Vim syntax file
" Language:	Neron C (Echelon C Variant)
" Maintainer:	Florian Delizy <florian.delizy@gmail.com>
" LastChange:	2008/05/10
" Comment:	Neuron C is only a C language extended with some commands
" 		hence we source the C language syntax and extend it...

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
   syntax clear
elseif exists("b:current_syntax")
   finish
endif

" Read the Ada syntax to start with
if version < 600
   so <sfile>:p:h/c.vim
else
   runtime! syntax/c.vim
   unlet b:current_syntax
endif

syn keyword cStatement when priority to

syn keyword cType output serial baud input mtimer stimer out in msg_tag 
syn keyword cType sci boolean far s32_type bit domain_struct

syn match cLabel display "IO_[0-9]\{1,2}"

" Neuron C does not support #if 0 directive ...
syn match cError display "#if 0"

syn keyword ncEvent timer_expires reset io_in_ready io_changes msg_arrives

syn keyword ncFunctions io_out io_in io_in_request io_iena s32_inc io_out_request
syn keyword ncFunctions sci_abort io_select io_idis s32_gt s32_from_ascii s32_add
syn keyword ncFunctions s32_type s32_minus_one s32_from_long s32_from_ulong s32_to_ascii

syn keyword ncMacro bind_info s32_zero

hi def link ncEvent Function
hi def link ncFunctions Function
hi def link ncMacro Macro


let b:current_syntax = "nc"
