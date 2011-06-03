" Vim syntax file
" Language:	SVF - Serial Vector Format, Revision E, 8 March 1999
" Maintainer:	Sebastian Witt <se.witt@gmx.net>
" Last Change:	2011 Feb 21


" Read the C syntax to start with
runtime! syntax/c.vim
unlet b:current_syntax

" SVF comments
syntax region  svfComment	start="!" end="$" keepend

" SVF commands
syn keyword svfCommands     ENDDR ENDIR FREQUENCY HDR HIR PIO PIOMAP RUNTEST SDR SIR STATE TDR TIR TRST

" SVF TAP state names
syn keyword svfStates       RESET IDLE DRSELECT DRCAPTURE DRSHIFT DREXIT1 DRPAUSE DREXIT2
                          \ DRUPDATE IRSELECT IRCAPTURE IRSHIFT IREXIT1 IRPAUSE IREXIT2 IRUPDATE

" SVF command parameters
syn keyword svfParams       TDI TDO MASK SMASK MAXIMUM ENDSTATE ON OFF Z ABSENT

" Default highlighting
command -nargs=+ HiLink hi def link <args>
  HiLink svfComment      Comment
  HiLink svfCommands     Function
  HiLink svfStates       Constant
  HiLink svfParams       Type
delcommand HiLink

let b:current_syntax = "svf"

" vim: ts=4
