" Vim global plugin for autoloading cscope databases.
" Last Change: Fri Nov 30 17:03:47 CST 2001
" Maintainer: Michael Conrad Tilsra <tadpol@tadpol.org>
" Revision: 0.2

if exists("loaded_autoload_cscope")
	finish
endif
let loaded_autoload_cscope = 1

let s:save_cpo = &cpo
set cpo&vim

" If you set this to anything other than 1, the menu and macros will not be
" loaded.  Useful if you have your own that you like.  Or don't want my stuff
" clashing with any macros you've made.
if !exists("g:autocscope_menus")
  let g:autocscope_menus = 1
endif

"==
" windowdir
"  Gets the directory for the file in the current window
"  Or the current working dir if there isn't one for the window.
"  FIXME This still doesn't always work.  If you open a buffer with 
"        gvim dir/file.c it ony gets `dir' and not the stuff before...
function s:windowdir()
  if winbufnr(0) == -1
    return getcwd()
  endif
  let wndr = bufname(winbufnr(0))
  let fr = match(wndr, "/[^/]*$")
  if fr == -1
    return getcwd()
  endif
  return strpart(wndr, 0, fr)
endfunc
"
"==
" Find_in_parent
" find the file argument and returns the path to it.
" Starting with the current working dir, it walks up the parent folders
" until it finds the file, or it hits the stop dir.
" If it doesn't find it, it returns "Nothing"
function s:Find_in_parent(fln,flsrt,flstp)
  let here = a:flsrt
  while ( strlen( here) > 0 )
    if filereadable( here . "/" . a:fln )
      return here
    endif
    let fr = match(here, "/[^/]*$")
    if fr == -1
      break
    endif
    let here = strpart(here, 0, fr)
    if here == a:flstp
      break
    endif
  endwhile
  return "Nothing"
endfunc
"
"==
" Cycle_macros_menus
"  if there are cscope connections, activate that stuff.
"  Else toss it out.
"  TODO Maybe I should move this into a seperate plugin?
let s:menus_loaded = 0
function s:Cycle_macros_menus()
  if g:autocscope_menus != 1
    return
  endif
  if cscope_connection()
    if s:menus_loaded == 1
      return
    endif
    let s:menus_loaded = 1
    set csto=0
    set cst
    silent! map <unique> <C-\>s :cs find c <C-R>=expand("<cword>")<CR><CR>
    silent! map <unique> <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
    silent! map <unique> <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>
    silent! map <unique> <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
    silent! map <unique> <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
    silent! map <unique> <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
    silent! map <unique> <C-\>f :cs find f <C-R>=expand("<cword>")<CR><CR>
    silent! map <unique> <C-\>i :cs find i <C-R>=expand("<cword>")<CR><CR>
    if has("menu")
      nmenu &Cscope.Find.Symbol<Tab><c-\>s
        \ :cs find s <C-R>=expand("<cword>")<CR><CR>
      nmenu &Cscope.Find.Definition<Tab><c-\>g
        \ :cs find g <C-R>=expand("<cword>")<CR><CR>
      nmenu &Cscope.Find.Called<Tab><c-\>d
        \ :cs find d <C-R>=expand("<cword>")<CR><CR>
      nmenu &Cscope.Find.Calling<Tab><c-\>c
        \ :cs find c <C-R>=expand("<cword>")<CR><CR>
      nmenu &Cscope.Find.Assignment<Tab><c-\>t
        \ :cs find t <C-R>=expand("<cword>")<CR><CR>
      nmenu &Cscope.Find.Egrep<Tab><c-\>e
        \ :cs find e <C-R>=expand("<cword>")<CR><CR>
      nmenu &Cscope.Find.File<Tab><c-\>f
        \ :cs find f <C-R>=expand("<cword>")<CR><CR>
      nmenu &Cscope.Find.Including<Tab><c-\>i
        \ :cs find i <C-R>=expand("<cword>")<CR><CR>
"      nmenu &Cscope.Add :cs add 
"      nmenu &Cscope.Remove  :cs kill 
      nmenu &Cscope.Reset :cs reset<cr>
      nmenu &Cscope.Show :cs show<cr>
      " Need to figure out how to do the add/remove. May end up writing
      " some container functions.  Or tossing them out, since this is supposed
      " to all be automatic.
    endif
  else
    let s:menus_loaded = 0
    set nocst
    silent! unmap <C-\>s
    silent! unmap <C-\>g
    silent! unmap <C-\>d
    silent! unmap <C-\>c
    silent! unmap <C-\>t
    silent! unmap <C-\>e
    silent! unmap <C-\>f
    silent! unmap <C-\>i
    if has("menu")  " would rather see if the menu exists, then remove...
      silent! nunmenu Cscope
    endif
  endif
endfunc
"
"==
" Unload_csdb
"  drop cscope connections.
function s:Unload_csdb()
  if exists("b:csdbpath")
    if cscope_connection(3, "out", b:csdbpath)
      let save_csvb = &csverb
      set nocsverb
      exe "cs kill " . b:csdbpath
      set csverb
      let &csverb = save_csvb
    endif
  endif
endfunc
"
"==
" Cycle_csdb
"  cycle the loaded csccope db.
function s:Cycle_csdb()
  if has("cscope")
    if exists("b:csdbpath")
      if cscope_connection(3, "out", b:csdbpath)
        return
        "it is already loaded. don't try to reload it.
      endif
    endif
    let newcsdbpath = s:Find_in_parent("cscope.out",s:windowdir(),$HOME)
"    echo "Found cscope.out at: " . newcsdbpath
"    echo "Windowdir: " . s:windowdir()
    if newcsdbpath != "Nothing"
      let b:csdbpath = newcsdbpath
      if !cscope_connection(3, "out", b:csdbpath)
        let save_csvb = &csverb
        set nocsverb
        exe "cs add " . b:csdbpath . "/cscope.out " . b:csdbpath
        set csverb
        let &csverb = save_csvb
      endif
      "
    else " No cscope database, undo things. (someone rm-ed it or somesuch)
      call s:Unload_csdb()
    endif
  endif
endfunc

" auto toggle the menu
augroup autoload_cscope
 au!
 au BufEnter *.[chly]  call <SID>Cycle_csdb() | call <SID>Cycle_macros_menus()
 au BufEnter *.cc      call <SID>Cycle_csdb() | call <SID>Cycle_macros_menus()
 au BufUnload *.[chly] call <SID>Unload_csdb() | call <SID>Cycle_macros_menus()
 au BufUnload *.cc     call <SID>Unload_csdb() | call <SID>Cycle_macros_menus()
augroup END

let &cpo = s:save_cpo
