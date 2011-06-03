" File:		searchInRuntime.vim 
" Author:	Luc Hermitte <EMAIL:hermitte@free.fr>
" 		<URL:http://hermitte.free.fr/vim>
" Last Update:  03rd apr 2002
" Version:	1.1
"
" Purpose:	Search a file in the runtime path, $PATH, or any other
"               environment variable, and execute an Ex command on it.
" History: {{{
"	Version 1.1
"	(*) Support the '&verbose' option :
"	     >= 0 -> display 'no file found'.
"	     >= 2 -> display the list of files found.
"	     >= 3 -> display the list of directories searched.
"	(*) SearchInPATH : like SearchInRuntime, but with $PATH
"	(*) SearchInENV : work on any list of directories defined in an
"	    environment variable.
"	(*) Define the classical debug command : Echo
"	(*) Contrary to 'runtime', the search can accept absolute paths ; 
"	    for instance, 
"	    	runtime! /usr/local/vim/share/*.vim 
"	    is not valid while 
"	    	SearchInRuntime runtime /usr/local/vim/share/*.vim 
"	    is accepted.
"	
"	Version 1.0 : initial version
" }}}
"
" Todo: {{{
" 	(*) Should be able to interpret absolute paths stored in environment
" 	    variables ; e.g: SearchInRuntime Echo $VIM/*vimrc*
" 	(*) Absolute paths should not shortcut the order of the file globing 
" 	    patterns ; see: SearchInENV! $PATH Echo *.sh /usr/local/vim/*
" }}}
"
" ========================================================================

if exists("g:searchInRuntime_vim") | finish | endif
let g:searchInRuntime_vim = 1

" ========================================================================
" Commands {{{
command! -nargs=+ -complete=file -bang
      \       SearchInRuntime	call <SID>SearchInRuntime("<bang>",  <f-args>)
command! -nargs=+ -complete=file -bang
      \       SearchInENV	call <SID>SearchInENV("<bang>",  <f-args>)
command! -nargs=+ -complete=file -bang
      \       SearchInPATH	call <SID>SearchInPATH("<bang>",  <f-args>)

if !exists('!Echo')
  command! -nargs=+ Echo echo "<args>"
endif

" }}}
" ========================================================================
" Functions {{{

function! s:SearchIn(do_all, cmd, rpath, ...) " {{{
  " Loop on runtimepath : build the list of files
  let rp = a:rpath
  let f = ''
  let firstTime = 1
  while strlen(rp) != 0
    let r  = matchstr(rp, '^[^,]*' )."/"
    let rp = substitute(rp, '.\{-}\(,\|$\)', '', '')
    if &verbose >= 3 | echo "Directory searched: [" . r. "]\n" | endif
      
    " Loop on arguments
    let i = 1
    while i <= a:0
      if a:{i} =~? '^\(/\|[a-z]:[\\/]\)' " absolute path
	if firstTime
	  if &verbose >= 3 | echo "Absolute path : [" . glob(a:{i}). "]\n" | endif
	  let f = f . glob(a:{i}) . "\n"
	endif
      else
	let f = f . glob(r.a:{i}). "\n"
	"echo a:{i} . " -- " . glob(r.a:{i})."\n"
	"echo a:{i} . " -- " . f."\n"
      endif
      let i = i + 1
    endwhile
    let firstTime = 0
  endwhile
  "
  " Execute the command on the matching files
  let foundOne = 0
  while strlen(f) != 0
    let ff = matchstr(f, "^[^\n]*")
    let f  = substitute(f, '.\{-}\('."\n".'\|$\)', '', '')
    if filereadable(ff)
      if &verbose >= 2 | echo "Action on: [" . ff . "]\n" | endif
      exe a:cmd." ".ff
      if !a:do_all | return | endif
      let foundOne = 1
    endif
  endwhile
  if &verbose > 0 && !foundOne " {{{
    let msg = "not found : « "
    let i = 1
    while i <= a:0
      let msg = msg. a:{i} . " "
      let i = i + 1
    endwhile
    echo msg."»"
  endif " }}}
endfunction
" }}}

function! s:SearchInRuntime(bang, cmd, ...) "{{{
  let do_all = a:bang == "!"
  let i = 1
  let a = ''
  while i <= a:0
    let a = a.",'".a:{i}."'"
    let i = i + 1
  endwhile
  exe 'call <sid>SearchIn(do_all, a:cmd, &runtimepath' .a.')'
endfunction "}}}

function! s:SearchInPATH(bang, cmd, ...) "{{{
  let do_all = a:bang == "!"
  let i = 1
  let a = ''
  while i <= a:0
    let a = a.",'".a:{i}."'"
    let i = i + 1
  endwhile
  let p = substitute($PATH, ';', ',', 'g')
  exe "call <sid>SearchIn(do_all, a:cmd,'". p ."'".a.")"
endfunction "}}}

function! s:SearchInENV(bang, env, cmd, ...) "{{{
  let do_all = a:bang == "!"
  let i = 1
  let a = ''
  while i <= a:0
    let a = a.",'".a:{i}."'"
    let i = i + 1
  endwhile
  exe "let p = substitute(".a:env.", ';', ',', 'g')"
  exe "call <sid>SearchIn(do_all, a:cmd,'". p ."'".a.")"
endfunction "}}}

" }}}
" ========================================================================
" vim60: set foldmethod=marker:
