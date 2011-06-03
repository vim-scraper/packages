" Comceal.vim 
" :Conceal 'em damn comments !  
" Author: Raghavendra Prabhu - rprabhu AT wnohang DOT net 
" Git: git.wnohang.net
" we need the conceal feature (vim ≥ 7.3)
" To be put under <plugin_name>/after/plugin/ if you use vundle or any another
" convenient location but should be sourced in the after path

"Variables you can set 
" 1. g:comceal_char -- character which can be used in place of comment -- for
"                       more check :help ccchar
" 2. g:comceal_list -- conceal levels to toggle between when using the Comceal command --
"                       for more check :help conceallevel
" 3. g:comceal_default -- The comment string to assume if the file type is not
"                       recognised by vim
" 4. g:comceal_disabled -- To disable this plugin :(
		" Test
if exists("g:comceal_disabled") && g:comceal_disabled == 1
  finish
endif

if exists("g:comceal_version")
  finish
endif

" Version number
let g:comceal_version = "0.0.2"
if !has('conceal') || !has('folding') || &cp
    echoerr "Either conceal or folding feature is missing: check vim --version output"
    finish
endif

if !exists("g:comceal_char")
	let g:comceal_char=">"
endif

if !exists("g:comceal_list")
	let g:comceal_list = [0,1,3]
endif

"  Check if comment is a multiline (C-style) or single line (python etc)
fun! s:singleComment()
    return (stridx(&commentstring,'%s') == strlen(&commentstring) - 2)
endfun


fun! s:matchComment()
    if empty(&ft) && exists("g:comceal_default")
        let l:commentstr = g:comceal_default
    else
        let l:commentstr = &commentstring
    endif
    let l:pattern = s:singleComment() ? '.*' : '\\\_.\\{-\}'
    let s:counter = index(g:comceal_list,&conceallevel)
    let l:commentstr = substitute(escape(l:commentstr,'"*/'),'\s*','','g')
    let s:commentString = '^\s*' . substitute(l:commentstr,'%s',l:pattern,'')
    exe 'syntax match matchComment "' . s:commentString .'" conceal cchar='.g:comceal_char
    hi! link matchComment Comment
endfun 

fun! s:toggle(counter)
    if empty(a:counter)
        let s:counter = (s:counter + 1) % len(g:comceal_list)
    else
        let s:counter = a:counter
    endif
    exe 'setlocal conceallevel ='.g:comceal_list[s:counter] 
    echo s:commentString
endfun  

augroup ComcealVimEnter
    autocmd!
    autocmd VimEnter * call s:matchComment()
augroup End

command! -nargs=? Comceal :call s:toggle(<q-args>)
" vim: set ts=4 sw=4 foldmethod=syntax tw=80 : 
