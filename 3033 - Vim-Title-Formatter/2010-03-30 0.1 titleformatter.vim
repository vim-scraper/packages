" Title        : Vim useful window title formatter
" Author       : Densel Santhmayor <denselm AT gmail DOT com>
" Last Change  : March 29, 2010
" Version      : 0.1

command! -nargs=1 SetVimTitleString :call UpdateVimCurrentTitle("<args>")
command! -nargs=0 ClearVimTitleString :call ClearVimCurrentTitle()

function! UpdateVimCurrentTitle(windowName)
    let g:vim_session_name = a:windowName
    call UpdateVimTitleString()
endfunction

function! ClearVimCurrentTitle()
    if exists ("g:vim_session_name") 
        unlet g:vim_session_name
    endif
    call UpdateVimTitleString()
endfunction

" Function to set the titlestring to include current Window name if specified
" OR servername if available 
function! UpdateVimTitleString()
    if exists ("g:vim_session_name") && g:vim_session_name != "" 
        let prependString = " " . g:vim_session_name . " | "
    elseif exists ("v:servername") && v:servername != ""
        let prependString = " " . v:servername . " | "
    else 
        let prependString = ""
    endif
    let &titlestring = prependString . expand("%:t") . " (".expand("%:p:h") . ")"       
endfunction

au! BufEnter * call UpdateVimTitleString()
au! BufWinEnter * call UpdateVimTitleString()
