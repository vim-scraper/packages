" CoremoSearch -- A simple simultaneous search script.
"
" Maintainer: Shuhei Kubota <kubota.shuhei@gmail.com>
" Description:
"   This script provides simultaneous search functionality.
"
"   You can add a string that you want to search and remove a string.
"
" Usage:
"   A. Adding a word (like asterisk(*) keystroke)
"       1. Place the cursor on the word that you want to search.
"       2. Press <C-@> or :CoremoSearchAdd
"
"   B. Adding a selected string
"       1. In visual mode, select the string that you want to search.
"       2. Press <C-@> or :CoremoSearchAddV
"
"   A'. Removing a word
"       1. Place the cursor on the word.
"       2. Press <Learder><C-@> or :CoremoSearchRemove
"           (in most cases, <Leader> equals to backslash(\) keystroke)
"
"   B'. Removing a selected string
"       1. In visual mode, select the string.
"       2. Press <Learder><C-@> or :CoremoSearchRemoveV
"           (in most cases, <Leader> equals to backslash(\) keystroke)
"
" Last Change: 29-Mar-2008

command!  -range  CoremoSearchAdd      call <SID>CoremoSearch_add()
command!  -range  CoremoSearchAddV     call <SID>CoremoSearch_addV()
command!  -range  CoremoSearchRemove   call <SID>CoremoSearch_remove()
command!  -range  CoremoSearchRemoveV  call <SID>CoremoSearch_removeV()

nnoremap  <C-@>          :CoremoSearchAdd<CR>
vnoremap  <C-@>          :CoremoSearchAddV<CR>
nnoremap  <Leader><C-@>  :CoremoSearchRemove<CR>
vnoremap  <Leader><C-@>  :CoremoSearchRemoveV<CR>

function! s:CoremoSearch_addV()
    let old_a = @a

    execute "normal \<ESC>"
    normal gv"ay

    echo 'Coremo Search: ' . @a
    call s:CoremoSearch__addInner(s:CoremoSearch__escape(@a))

    let @a = old_a
endfunction

function! s:CoremoSearch_add()
    let old_a = @a

    execute "normal \<ESC>"
    if stridx(" \t　\r\n", getline('.')[col('.') - 1]) != -1
        execute "normal vaw\<ESC>"
    endif
    normal viw"ay

    echo 'Coremo Search: ' . @a
    call s:CoremoSearch__addInner(s:CoremoSearch__escape(@a))

    let @a = old_a
endfunction

function! s:CoremoSearch_removeV()
    let old_a = @a

    execute "normal \<ESC>"
    normal gv"ay

    echo 'Forgot: ' . @a
    call s:CoremoSearch__removeInner(s:CoremoSearch__escape(@a))

    let @a = old_a
endfunction

function! s:CoremoSearch_remove()
    let old_a = @a

    execute "normal \<ESC>"
    if stridx(" \t　\r\n", getline('.')[col('.') - 1]) != -1
        execute "normal vaw\<ESC>"
    endif
    normal viw"ay

    echo 'Forgot: ' . @a
    call s:CoremoSearch__removeInner(s:CoremoSearch__escape(@a))

    let @a = old_a
endfunction

function! s:CoremoSearch__addInner(expr)
    let all = sort(split(@/, '\\|'))
    if index(all, a:expr) == -1
        call add(all, a:expr)
    endif
    let @/ = join(all, '\|')
endfunction

function! s:CoremoSearch__removeInner(expr)
    let all = sort(split(@/, '\\|'))
    let idx = max([index(all, a:expr), index(all, '\<' . a:expr . '\>')])
    if idx != -1
        call remove(all, idx)
    endif
    let @/ = join(all, '\|')
endfunction

function! s:CoremoSearch__escape(expr)
    return escape(a:expr, '\$.*/[]^')
endfunction

" vim: set et ff=unix fileencoding=utf-8 sts=4 sw=4 ts=4 : 
