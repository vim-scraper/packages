" SeeTab:	displays a bar for each tab
"  Author:	Charles E. Campbell, Jr.
"  Date:	Apr 25, 2003
"  Version:	2
"
"  Usage:  :SeeTab    (toggles tab-bars)
"
"  Variables:
"     g:SeeTabFG : nominal foreground color (default: magenta)
"     g:SeeTabBG : nominal background color (default: black)

" allow user to bypass loading, also implements only-load-once
if exists("g:SeeTabLoaded")
 finish
endif
let g:SeeTabLoaded= 1

" user may override either or both of these colors in his/her <.vimrc>
if !exists("g:SeeTabCtermFG")
 let g:SeeTabCtermFG="magenta"
endif
if !exists("g:SeeTabCtermBG")
 let g:SeeTabCtermBG="black"
endif
if !exists("g:SeeTabGuiFG")
 let g:SeeTabGuiFG="magenta"
endif
if !exists("g:SeeTabGuiBG")
 let g:SeeTabGuiBG="black"
endif

" ---------------------------------------------------------------------

" SeeTab: toggles between showing tabs and using standard listchars
fu! s:SeeTab()

  if !exists("g:SeeTabEnabled")
   " -----------------------
   " Make tab bar(s) visible
   " -----------------------
   let g:SeeTabEnabled= 1

   " record original SpecialKey, change SpecialKey
   let regA= @a
   redir @a
   silent! hi SpecialKey
   redir END
   let g:SeeTabSpecialKey= @a
   let @a                = regA
   hi clear SpecialKey

   if &et
    syn clear
    syn match SeeTabMatch   /^\s\+/ contains=SeeTabBar
    let tsm1= &ts - 1
    exe 'syn match SeeTabBar        /  \{'.tsm1.'}/hs=s,he=s+1 contained'
    hi link SeeTabBar SpecialKey
    exe 'silent! hi SpecialKey ctermfg='.g:SeeTabCtermBG.' ctermbg='.g:SeeTabCtermFG.' guifg=.'g:SeeTabGuiBG.' guibg='.g:SeeTabGuiFG
   else
    let g:SeeTab_list      = &list
    let g:SeeTab_listchars = &listchars
   
	" note that list uses SpecialKey highlighting
    set list
    set listchars=tab:\|\ 
    exe 'silent! hi SpecialKey cterm=reverse gui=reverse ctermfg='.g:SeeTabCtermBG.' ctermbg='.g:SeeTabCtermFG.' guifg='.g:SeeTabGuiBG.' guibg=.'g:SeeTabGuiFG
   endif

  else
   " -------------------------
   " restore display to normal
   " -------------------------
   silent! exe "hi ".substitute(g:SeeTabSpecialKey,'xxx','','e')
   if &et
	syn clear SeeTabMatch SeeTabBar
	unlet g:SeeTabEnabled
   else
    let &list      = g:SeeTab_list
    let &listchars = &listchars
    " restore SpecialKey
    unlet g:SeeTabEnabled g:SeeTab_list g:SeeTab_listchars
   endif
  endif
endfunc
com! -nargs=0 SeeTab :call <SID>SeeTab()
" ---------------------------------------------------------------------
