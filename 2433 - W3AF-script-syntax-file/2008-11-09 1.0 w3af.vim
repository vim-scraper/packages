" Vim syntax file
" Language:	w3af file
" Maintainer: Pento <naplanetu@gmail.com>
" Last change: 2008 Oct 12

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish 
endif

syntax sync fromstart

syn keyword w3afStatement	set back start assert plugins exploit profiles exit help
syn keyword w3afSettings   target
syn keyword w3afSettings   http-settings
syn keyword w3afSettings   misc-settings
syn keyword w3afPluginGroups audit bruteforce grep evasion output mangle discovery
syn match   w3afComment "#.*$"

if version >= 508 || !exists("did_w3af_syn_inits")
  if version <= 508
    let did_w3af_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

" The default methods for highlighting.  Can be overridden later
  HiLink w3afStatement	Statement
  HiLink w3afPluginGroups	Define
  HiLink w3afComment      Comment
  HiLink w3afSettings Underlined
  delcommand HiLink
endif

let b:current_syntax = 'w3af'
