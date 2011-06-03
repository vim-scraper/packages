" File Name: fluxkeys.vim
" Maintainer: Mathias Gumz <akira at fluxbox dot org>
" Original Date: 040127 00:40:33 
" Last Update: 050624 05:44:19 
" Description: fluxbox key syntax
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif

" 
syntax keyword fluxboxActionNames ArrangeWindows  contained
syntax keyword fluxboxActionNames Bindkey  contained
syntax keyword fluxboxActionNames Close  contained
syntax keyword fluxboxActionNames CommandDialog  contained
syntax keyword fluxboxActionNames Deiconify  contained
syntax keyword fluxboxActionNames DetachClient  contained
syntax keyword fluxboxActionNames Export  contained
syntax keyword fluxboxActionNames Exec  contained
syntax keyword fluxboxActionNames ExecCommand  contained
syntax keyword fluxboxActionNames Execute  contained
syntax keyword fluxboxActionNames Exit  contained
syntax keyword fluxboxActionNames FocusUp  contained
syntax keyword fluxboxActionNames FocusDown  contained
syntax keyword fluxboxActionNames FocusLeft  contained
syntax keyword fluxboxActionNames FocusRight  contained
syntax keyword fluxboxActionNames Fullscreen  contained
syntax keyword fluxboxActionNames Iconify  contained
syntax keyword fluxboxActionNames KillWindow  contained
syntax keyword fluxboxActionNames LeftWorkspace  contained
syntax keyword fluxboxActionNames Lower  contained
syntax keyword fluxboxActionNames LowerLayer  contained
syntax keyword fluxboxActionNames MacroCmd  contained
syntax keyword fluxboxActionNames Maximize  contained
syntax keyword fluxboxActionNames MaximizeHorizontal  contained
syntax keyword fluxboxActionNames MaximizeVertical  contained
syntax keyword fluxboxActionNames MaximizeWindow  contained
syntax keyword fluxboxActionNames Minimize  contained
syntax keyword fluxboxActionNames MinimizeWindow  contained
syntax keyword fluxboxActionNames MoveTo  contained
syntax keyword fluxboxActionNames Move  contained
syntax keyword fluxboxActionNames MoveDown  contained
syntax keyword fluxboxActionNames MoveLeft  contained
syntax keyword fluxboxActionNames MoveRight  contained
syntax keyword fluxboxActionNames MoveTabLeft  contained
syntax keyword fluxboxActionNames MoveTabRight  contained
syntax keyword fluxboxActionNames MoveUp  contained
syntax keyword fluxboxActionNames Next\Group  contained
syntax keyword fluxboxActionNames NextTab  contained
syntax keyword fluxboxActionNames NextWindow  contained
syntax keyword fluxboxActionNames NextWorkspace  contained
syntax keyword fluxboxActionNames PrevGroup  contained
syntax keyword fluxboxActionNames PrevTab  contained
syntax keyword fluxboxActionNames PrevWindow  contained
syntax keyword fluxboxActionNames PrevWorkspace  contained
syntax keyword fluxboxActionNames Quit  contained
syntax keyword fluxboxActionNames Raise  contained
syntax keyword fluxboxActionNames RaiseLayer  contained
syntax keyword fluxboxActionNames Reconfig  contained
syntax keyword fluxboxActionNames Reconfigure  contained
syntax keyword fluxboxActionNames ReloadStyle  contained
syntax keyword fluxboxActionNames ResizeTo  contained
syntax keyword fluxboxActionNames Resize  contained
syntax keyword fluxboxActionNames ResizeHorizontal  contained
syntax keyword fluxboxActionNames ResizeVertical  contained
syntax keyword fluxboxActionNames Restart  contained
syntax keyword fluxboxActionNames RightWorkspace  contained
syntax keyword fluxboxActionNames RootMenu  contained
syntax keyword fluxboxActionNames SaveRc  contained
syntax keyword fluxboxActionNames Setenv  contained
syntax keyword fluxboxActionNames SetHead  contained
syntax keyword fluxboxActionNames SendToWorkspace  contained
syntax keyword fluxboxActionNames SendToNextWorkspace  contained
syntax keyword fluxboxActionNames SendToPrevWorkspace  contained
syntax keyword fluxboxActionNames SetStyle  contained
syntax keyword fluxboxActionNames SetWorkspacename  contained
syntax keyword fluxboxActionNames SetWorkspacenameDialog  contained
syntax keyword fluxboxActionNames SetResourcevalue  contained
syntax keyword fluxboxActionNames SetResourcevalueDialog  contained
syntax keyword fluxboxActionNames Shade  contained
syntax keyword fluxboxActionNames ShadeWindow  contained
syntax keyword fluxboxActionNames ShowDesktop  contained
syntax keyword fluxboxActionNames Stick  contained
syntax keyword fluxboxActionNames StickWindow  contained
syntax keyword fluxboxActionNames Tab  contained
syntax keyword fluxboxActionNames TakeToWorkspace  contained
syntax keyword fluxboxActionNames TakeToNextWorkspace  contained
syntax keyword fluxboxActionNames TakeToPrevWorkspace  contained
syntax keyword fluxboxActionNames ToggleDecor  contained
syntax keyword fluxboxActionNames WindowMenu  contained
syntax keyword fluxboxActionNames Workspace  contained
syntax keyword fluxboxActionNames Workspace1  contained
syntax keyword fluxboxActionNames Workspace2  contained
syntax keyword fluxboxActionNames Workspace3  contained
syntax keyword fluxboxActionNames Workspace4  contained
syntax keyword fluxboxActionNames Workspace5  contained
syntax keyword fluxboxActionNames Workspace6  contained
syntax keyword fluxboxActionNames Workspace7  contained
syntax keyword fluxboxActionNames Workspace8  contained
syntax keyword fluxboxActionNames Workspace9  contained
syntax keyword fluxboxActionNames Workspace10  contained
syntax keyword fluxboxActionNames Workspace11  contained
syntax keyword fluxboxActionNames Workspace12  contained
syntax keyword fluxboxActionNames WorkspaceMenu  contained

" modifier - keys
syntax keyword fluxboxModifiers Control Shift  contained
syntax keyword fluxboxModifiers Mod1 Mod2 Mod3 Mod4 Mod5  contained
syntax keyword fluxboxModifiers None  contained

" reference corners
syntax keyword fluxboxParameterRefCorner UpperLeft Upper UpperRight contained
syntax keyword fluxboxParameterRefCorner Left Right contained
syntax keyword fluxboxParameterRefCorner LowerLeft Lower LowerRight contained

" parameter numbers
syntax match   fluxboxParameterNumber /\([+-]\)*\d\+/ contained


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"  match the right parts
syntax match   fluxboxParameter /.*/ contained contains=fluxboxParameterRefCorner,fluxboxParameterNumber

" anything with an unknown ActionName is colored Error
syntax match   fluxboxAction /\w\+/ contained contains=fluxboxActionNames nextgroup=fluxboxParameter
syntax match   fluxboxExec /Exec\(ute\|Command\)*\s\+.*$/ contained contains=fluxboxActionNames

" anything but a valid modifier is colored Error
syntax match   fluxboxKeyStart /^\w\+/  contained contains=fluxboxModifiers

" anything but a comment or a valid key line is colored Error
syntax match   fluxboxNoKeyline /.\+$/ display skipwhite
syntax region  fluxboxKeys start=/\w\+/ end=/.\{-}:/he=e-1 contains=fluxboxKeyStart,fluxboxModifiers nextgroup=fluxboxExec,fluxboxAction oneline
syntax match   fluxboxComment /[#!].*$/ display 

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" coloring

highlight link fluxboxNoKeyline Error
highlight link fluxboxAction Error
highlight link fluxboxKeyStart Error

highlight link fluxboxActionNames Type 
highlight link fluxboxComment Comment
highlight link fluxboxModifiers Macro
highlight link fluxboxKeys Number
highlight link fluxboxExec String
highlight link fluxboxParameterNumber Conditional

syntax sync fromstart

let b:current_syntax = 'fluxkeys'

