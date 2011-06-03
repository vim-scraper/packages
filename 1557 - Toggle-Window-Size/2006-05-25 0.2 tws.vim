" Toggle Window Size
" Based on Tip #1110 by Jang Junyeong (less efficient, but works in Linux)
" This version has only been tested in Linux GTK under KDE (afaik)
" 
" has not been tested in any other environment afaik
" author: John C Kendall, jkendall@technologist.com
" Version 0.2
" 5/25/06 -- fixed to use variables instead of hard-coded values for columns and lines
"            Thanks to Ilya at po4ta.com for the tip!
"            Enhanced to keep track of window position for restore

" Change these values to whatever is appropriate for your display/resolution/font
"   setting to 999 should make it full screen
"
let g:twsMaxColumns = 200
let g:twsMaxLines = 92

function TWS_SaveWindowPos()
  let g:twsX = getwinposx()
  let g:twsY = getwinposy()
endfunction

" Save initial values
call TWS_SaveWindowPos()

" attempt to keep track of window position
" unfortunately, no "VimMoved" event is available, so this may not always work
" (however, it seems to work always in my testing...)
" cavet: if you change focus after maximizing, it will record the new position...
" Just comment out or delete if it annoys you
autocmd FocusGained * call TWS_SaveWindowPos()
autocmd FocusLost * call TWS_SaveWindowPos()

" If you would rather that the "restore" size track the current size of
" your gvim window, then move these lines into the TWS_SaveWindowPos function
let g:twsColumns = &columns
let g:twsLines = &lines

function ToggleWindowSize(act)
  if a:act == 2 || a:act == 0
    if &lines == g:twsLines
      if a:act != 0
        let g:twsX = getwinposx()
      endif
      let g:twsY = getwinposy()
      execute "set lines=" . g:twsMaxLines
    else
      execute "set lines=" . g:twsLines
      execute "winpos " . g:twsX . " " . g:twsY
    endif
  endif
  if a:act == 1 || a:act == 0
    if &columns == g:twsColumns
      if a:act != 0
        let g:twsY = getwinposy()
      endif
      let g:twsX = getwinposx()
      execute "set columns=" . g:twsMaxColumns
    else
      execute "set columns=" . g:twsColumns
      execute "winpos " . g:twsX . " " . g:twsY
    endif
  endif
endfunction

" Map to whatever keys you want
map <S-F1> :call ToggleWindowSize(2)<CR>
map <S-F2> :call ToggleWindowSize(1)<CR>
map <S-F3> :call ToggleWindowSize(0)<CR>


