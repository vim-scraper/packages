" http://www.vim.org/scripts/script.php?script_id=1429
" File: cfname.vim
" Author: Fabio Visona', Yakov Lerner 
" Version: 3.0a (2006-03-24)
"
" This is a simple (limited) script for C/C++ programmers.
" When editing source files with very long functions, it may happen the 
" programmer do not know which function the current line is in (e.g. 
" when using tags and jumping with Ctrl-] or jumping after a search). 
" Here are some shortcuts to be typed in normal mode: 
"    ff: shows the function prototype on the command line 
"    fb: jumps to the function beginning 
"    fe: jumps to the function end 
"    ft: jump back where the cursor was before typing 'fb' or 'fe'
"        (the first 'fb' or 'fe' if a sequence of them is typed)
"    fz: folds the function 
"    fo: unfolds the function      
" Also, the function name is visible between square brackets on the status 
" line, with automatic update. To disable/enable this feature on-the-go use 'fd'/'fs'.
" To disable it steadily, comment the statements 
" 'au BufRead,BufNewFile * CFunSetStatusByFileName' and
" 'au BufEnter * CFunSetStatusByFileName' by putting a " in front of them.
"
"
" ***KNOWN ISSUES***: 
" - The automatic update of the status bar may slow down or even block the cursor movements. 
"   This has been experienced with VIM running on a UNIX machine. Refer to the instructions above
"   to disable the status line update.
"
" - In files with comments like '// ... }' (with unmatched '{' or '}' brackets), the script does
"   not work well (however, from version 2.2 on, the cursor should behave properly). 
"   I think it will not be easy to fix this problem.
"
" 
" Now someone told me that taglist.vim already does the job of showing the function prototype,
" together with a lot more! I don't now how comes I missed it, since it is the most downloaded 
" script :)
" 
" 
" Revision history:
" 1.0: First revision
" 2.0: Added 'fz' and 'fo' fold functions; shown the function name on the status bar
" 2.1: Removed some ^M characters which led to warning messages under UNIX
" 2.2: - Added 'ft' to jump back where the cursor was before typing 'fb' or 'fe'
"      - Added missing restoring of the cursor position in 'CF_GetPrototype' when 
"      'searchpair' calls fail
" 3.0: The function name is now automatically displayed on the status bar only for *.c,
"      *.cpp or *.cc files, because the status bar update caused the scrolling of non-c 
"      files to slow down unnecessarily. These changes have been contributed by Yakov Lerner.
" 3.0a: - If the user has a non-standard status line, that is preserved and the function name information
"         is added to it (the status line is not changed to default);
"       - The status line is updated with the function name information only after a while if the
"         cursor position is held (it shows [...] before). That is useful if you feel window scrolling
"         is slowed down too much.
"


let g:CF_FunctionName = ""
let g:CF_FunctionEnd = 0 
let g:CF_MemRCsaved = 0
let g:CF_MemRow = 0
let g:CF_MemCol = 0
let g:CF_MemRow_GP = 0
let g:CF_MemCol_GP = 0
let s:InHoldCount=0
let s:RefreshCount=0


function! CF_UpdateFunctionNameForStatusBar()
    let s:RefreshCount = s:RefreshCount + 1
    " we want to make searches only when called from CursorHold
    if s:InHoldCount == s:RefreshCount-1 || s:RefreshCount%10000==0
       let s:InHoldCount=0   " allow search
    else
        return '...'         " wait until CursorHold event
    endif

    let lastfunrow = searchpair('{', '', '}', 'rn')
    if lastfunrow != g:CF_FunctionEnd	
      let g:CF_FunctionEnd = lastfunrow	     
      let g:CF_FunctionName = s:CF_GetPrototype(1)
    endif 
    return g:CF_FunctionName
endfunction


function! s:CF_SaveCursorPosition()
    if g:CF_MemRCsaved == 0 
      let g:CF_MemRCsaved = 1
      let g:CF_MemRow = line(".")
      let g:CF_MemCol = col(".")    
    endif  
endfunction


function! s:CF_SaveCursorPositionGetPrototype()
    let g:CF_MemRow_GP = line(".")
    let g:CF_MemCol_GP = col(".")    
endfunction


function! s:CF_RestoreCursorPosition()
    call cursor(g:CF_MemRow, g:CF_MemCol)
    let g:CF_MemRCsaved = 0
endfunction


function! s:CF_RestoreCursorPositionGetPrototype()
    call cursor(g:CF_MemRow_GP, g:CF_MemCol_GP)
endfunction


function! s:CF_GetPrototype(nameonly_or_fullprototype)
    "save current position
    call s:CF_SaveCursorPositionGetPrototype()
    let funname = ""
    let rownumber = searchpair('{', '', '}', 'r') 
    if searchpair('{', '', '}', 'b') == 0 
      call s:CF_RestoreCursorPositionGetPrototype()
      return funname
    endif	    
    if searchpair(')', '', '}', 'b') == 0
      call s:CF_RestoreCursorPositionGetPrototype()
      return funname
    endif
    if searchpair('(', '', ')', 'b') == 0 
      call s:CF_RestoreCursorPositionGetPrototype()
      return funname
    endif
    if searchpair('\<\i*\>', '', '(', 'b') == 0
      call s:CF_RestoreCursorPositionGetPrototype()
      return funname
    endif
    if a:nameonly_or_fullprototype == 1 
      let funname = matchstr(getline(line(".")), '\<\i*\>', col(".") - 1)
     else
      let funname = getline(line("."))
      echo funname
    endif  
    "jump back to the previous position
    call s:CF_RestoreCursorPositionGetPrototype()
    return funname
endfunction


function! s:CF_JumpFunctionStart()
    call s:CF_SaveCursorPosition()
    let rownumber = searchpair('{', '', '}', 'r')            
    let rownumber = searchpair('{', '', '}', 'b')            
endfunction


function! s:CF_JumpFunctionEnd()
    call s:CF_SaveCursorPosition()    
    let rownumber = searchpair('{', '', '}', 'r')        
endfunction


function! s:CF_EnableFunctionNameOnStatusBar()
    if exists("s:saved_statusline")| return | endif
    let s:saved_statusline=&statusline
    "set statusline=%<%f\ %h%m%r\ [%{CF_UpdateFunctionNameForStatusBar()}]%=%-14.(%l,%c%V%)\ %L    
    if &statusline == "" 
        let &statusline="%<%f %h%m%r%=%-14.(%l,%c%V%) %L"
    endif
    if &statusline =~# "%r" 
        let &statusline=substitute(&statusline, "%r", "%r [%{CF_UpdateFunctionNameForStatusBar()}]", "")
    else
        let &statusline="[%{CF_UpdateFunctionNameForStatusBar()}]".&statusline
    endif
endfunction

au CursorHold * let s:InHoldCount=s:RefreshCount


function! s:CF_DisableFunctionNameOnStatusBar()
    "set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %L
    if exists("s:saved_statusline")
        let &statusline=s:saved_statusline
        unlet s:saved_statusline
    endif
endfunction


command! -bar -nargs=0 CFunprototype :call s:CF_GetPrototype(0)
command! -bar -nargs=0 CFunjumpstart :call s:CF_JumpFunctionStart()
command! -bar -nargs=0 CFunjumpend :call s:CF_JumpFunctionEnd()
command! -bar -nargs=0 CFunjumpback :call s:CF_RestoreCursorPosition()
command! -bar -nargs=0 CFunEnablestatusinfo :call s:CF_EnableFunctionNameOnStatusBar()
command! -bar -nargs=0 CFunDisablestatusinfo :call s:CF_DisableFunctionNameOnStatusBar()
command! -bar -nargs=0 CFunSetStatusByFileName if expand('%') =~# '\.c$\|\.cpp$\|\.cc$' | CFunEnablestatusinfo | else | CFunDisablestatusinfo | endif

" before 2006-03-21, this was enabled for all files.
" after  2006-03-21, we enable it for *.c files, disable for all other files.
map Fd :CFunDisablestatusinfo<CR>
map Fs :CFunEnablestatusinfo<CR>
au BufRead,BufNewFile * CFunSetStatusByFileName
au BufEnter * CFunSetStatusByFileName
"[this was on before 2006-03-21] call s:CF_EnableFunctionNameOnStatusBar()


map Ff :CFunprototype<CR>
map Fb :CFunjumpstart<CR>
map Fe :CFunjumpend<CR>
map Ft :CFunjumpback<CR>
map Fz :CFunjumpend<CR>zf%
map Fo zo
map F? :echo "Ff show current funtion prototype\nFb to function beg\nFe to func end\nFt go back\nFs show current func name on status line\nFd disable\nFs enable "<cr>



