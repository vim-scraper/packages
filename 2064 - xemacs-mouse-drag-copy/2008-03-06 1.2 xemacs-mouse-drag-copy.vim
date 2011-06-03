" Vim code for .vimrc file
" Maintainer: Carl Mueller, cmlr@math.rochester.edu
" Last Change:	March 6, 2007
" Version:  1.2
"
"In xemacs, or emacs if you have it enabled, you can speed up copying and 
"pasting.  Select some text with the left mouse button, while pressing the 
"control key.  The selected text will be copied and pasted to the original 
"position of the cursor.  This saves time in moving the cursor.  Also, the
"same action with the shift key instead of the control key moves the
"selected text instead of copying it.  Put the following code in your .vimrc.

" Shift-left mouse drag moves text, xemacs style.
inoremap <S-LeftMouse> <C-R>=<SID>LeftMouseMap()<CR><LeftMouse>
inoremap <S-Leftdrag> <Leftdrag>
vnoremap <S-Leftdrag> <Leftdrag>
vnoremap <S-LeftRelease> <LeftRelease>x:call <SID>LeftReleaseMap()<CR><C-R>"

" Control-left mouse drag copies and pastes, xemacs style.
inoremap <C-LeftMouse> <C-R>=<SID>LeftMouseMap()<CR><LeftMouse>
inoremap <C-Leftdrag> <Leftdrag>
vnoremap <C-Leftdrag> <Leftdrag>
vnoremap <C-LeftRelease> <LeftRelease>y:call <SID>LeftReleaseMap()<CR><C-R>"
function! s:LeftMouseMap()
    let g:thisline=line('.')
    let g:thiscol=virtcol('.')
    if g:thiscol == strlen(getline(g:thisline))+1
	let g:atend=1
    else
	let g:atend=0
    endif
    return ""
endfunction
function! s:LeftReleaseMap()
    exe 'normal '.g:thisline.'G0'.g:thiscol.'|'
    if g:atend == 1
	startinsert!
    else
	startinsert
    endif
    return ""
endfunction
