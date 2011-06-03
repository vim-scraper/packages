" railstab.vim - Rails tabstop hacks
" Maintainer:   Tim Pope

" Usage:
"   let g:rails_tabstop=4
"
" This companion to rails.vim is for people who dislike the default
" 'shiftwidth' of 2.  When non-zero, all Rails files will have a |:retab|!
" done with 'tabstop' set to 2 on load, to convert the initial indent from
" spaces to tabs.  Then, 'tabstop' and 'shiftwidth' will be set to the
" option's value.  The process is reversed on write.  Thus, one can use a
" custom indent when editing files, yet conform to Rails conventions when
" saving them.  There is also a local buffer version of this option, to allow
" for things like:
"
"   autocmd User Rails if &ft == 'ruby' | let b:rails_tabstop = 4 | endif
"
" This option defaults to being unset entirely, which blocks the relevant code
" from even loading.  Setting it to zero will allow the code to load without
" actually enabling it (such that it might be enabled later), while setting it
" to a non-zero value enables the behavior.
"
" If instead of all this magic, you would prefer to just override this plugin's
" settings and use your own custom 'shiftwidth', adjust things manually in an
" autocommand: >
"
"   autocmd User Rails set sw=4 sts=4 noet
"
" This is highly discouraged: don't fight Rails.

" Exit quickly when:
" - this plugin was already loaded (or disabled)
" - when 'compatible' is set
if (exists("g:loaded_railstab") && g:loaded_railstab) || &cp
    finish
endif
let g:loaded_railstab = 1

let s:cpo_save = &cpo
set cpo&vim

function! s:tabstop()
  if !exists("b:rails_root")
    return 0
  elseif &filetype !~ '^\%(ruby\|eruby\|haml\|dryml\|liquid\|html\|css\|sass\|yaml\|javascript\)$'
    return 0
  elseif exists("b:rails_tabstop")
    return b:rails_tabstop
  elseif exists("g:rails_tabstop")
    return g:rails_tabstop
  endif
endfunction

function! s:breaktabs()
  let ts = s:tabstop()
  if ts
    if exists("s:retab_in_process")
      unlet s:retab_in_process
      let line = line('.')
      lockmarks silent! undo
      lockmarks exe line
    else
      let &l:tabstop = 2
      setlocal noexpandtab
      let mod = &l:modifiable
      setlocal modifiable
      let line = line('.')
      lockmarks retab!
      lockmarks exe line
      let &l:modifiable = mod
    endif
    let &l:tabstop = ts
    let &l:softtabstop = ts
    let &l:shiftwidth = ts
  endif
endfunction

function! s:fixtabs()
  let ts = s:tabstop()
  if ts && ! &l:expandtab && !exists("s:retab_in_process")
    let s:retab_in_process = 1
    let &l:tabstop = 2
    setlocal expandtab
    let line = line('.')
    lockmarks retab
    lockmarks exe line
    let &l:tabstop = ts
  endif
endfunction

augroup railsPluginTabstop
  autocmd!
  autocmd BufWritePost,BufReadPost * call s:breaktabs()
  autocmd BufWritePre              * call s:fixtabs()
augroup END

let &cpo = s:cpo_save

" vim:set ft=vim ts=8 sw=2 sts=2:
