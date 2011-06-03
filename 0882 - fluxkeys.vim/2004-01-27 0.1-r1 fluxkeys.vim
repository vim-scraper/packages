" File Name: fluxkeys.vim
" Maintainer: M.Gumz aka ak|ra (#fluxbox on freenode) <gumz at cs.uni-magdeburg.de>
" Original Date: 040127 00:40:33 
" Last Update: 040127 00:40:37 
" Description: fluxbox key syntax

" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syntax keyword fluxboxAction RootMenu WindowMenu WorkspaceMenu
syntax keyword fluxboxAction ExecCommand 
syntax keyword fluxboxAction MaximizeWindow MaximizeVertical MaximizeHorizontal
syntax keyword fluxboxAction Stick Minimize Close
syntax keyword fluxboxAction Resize Move MoveRight MoveLeft MoveUp MoveDown 
syntax keyword fluxboxAction CommandDialog Restart Reconfigure
syntax keyword fluxboxAction Workspace LeftWorkspace RightWorkspace
syntax keyword fluxboxAction ToggleDecor MacroCmd
syntax keyword fluxboxAction DetachClient PrevTab NextTab
syntax keyword fluxboxAction NextWindow PrevWindow
syntax keyword fluxboxAction SendToPrevWorkspace SendToNextWorkspace
syntax keyword fluxboxModifiers Control Shift 
syntax keyword fluxboxModifiers Mod1 Mod2 Mod3 Mod4 Mod5 
syntax keyword fluxboxModifiers None

syntax match fluxboxKeys /^.*:/he=e-1
syntax match fluxboxComment /[#!].*$/


highlight link fluxboxAction Type 
highlight link fluxboxComment Comment
highlight link fluxboxModifiers Macro
highlight link fluxboxKeys String
syntax sync fromstart

let b:current_syntax = 'fluxkeys'

