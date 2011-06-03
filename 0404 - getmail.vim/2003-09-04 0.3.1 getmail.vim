" Vim syntax file
" Language:	getmailrc - configuration file for getmail
" Maintainer:	Nikolai Nespor <nikolai.nespor@utanet.at>
" URL:		http://www.unet.univie.ac.at/~a9600989/vim/getmail.vim
" Last Change:	2003 09 04

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match gmParam /^\s*[a-zA-Z_ \t]\+=/ contains=gmKeywd
syn match gmSection /^\s*\[[^#]\+\]/
syn match gmDefSection /^\s*\[default\]\s*$/
syn match gmComment /#.*$/
syn keyword gmTrue 1
syn keyword gmFalse 0

syn keyword gmKeywd contained command_add_fromline
syn keyword gmKeywd contained delete delete_after envelope_recipient
syn keyword gmKeywd contained extension_depth extension_sep local
syn keyword gmKeywd contained max_message_size max_messages_per_session
syn keyword gmKeywd contained message_log no_delivered_to no_recived
syn keyword gmKeywd contained password port postmaster readall server 
syn keyword gmKeywd contained timeout username use_apop use_*env verbose

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_getmail_syn_inits")
  if version < 508
    let did_getmail_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink gmParam        Normal
  HiLink gmKeywd        Type
  HiLink gmSection      Statement
  HiLink gmDefSection   PreProc
  HiLink gmComment      Comment
  HiLink gmFalse        Constant
  HiLink gmTrue         Identifier
  delcommand HiLink
endif

let b:current_syntax = "getmail"

" vim: ts=8
