"In xemacs, or emacs if you have it enabled, you can speed up copying and 
"pasting.  Select some text with the left mouse button, while pressing the 
"control key.  The selected text will be copied and pasted to the original 
"position of the cursor.  This saves time in moving the cursor.  Put the 
"following code in your .vimrc.


" Control-left mouse button copies and pastes, xemacs style.
inoremap <C-LeftMouse> <C-R>=LeftMouseMap()<CR><LeftMouse>
inoremap <C-Leftdrag> <Leftdrag>
vnoremap <C-Leftdrag> <Leftdrag>
vnoremap <C-LeftRelease> <LeftRelease>y:call LeftReleaseMap()<CR><C-R>"
function! LeftMouseMap()
    let g:thisline=line('.')
    let g:thiscol=col('.')
    if g:thiscol == strlen(getline(g:thisline))+1
	let g:atend=1
    else
	let g:atend=0
    endif
    return ""
endfunction
function! LeftReleaseMap()
    exe 'normal '.g:thisline.'G0'.g:thiscol.'|'
    if g:atend == 1
	startinsert!
    else
	startinsert
    endif
endfunction

