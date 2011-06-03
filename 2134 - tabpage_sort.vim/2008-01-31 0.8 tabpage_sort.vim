" Title: tabpage_sort.vim : Sort vim7 tab page in 'buffer number' order.
" File: tabpage_sort.vim
" Author: Omi Taku (mail@nanasi.jp)
" URL: http://nanasi.jp/
" Version: 0.8
" Last Modified: February 01, 2008
"
" Overview
" --------
" sort vim7 tab page in 'buffer number' order.
"
" Installation
" ------------
" 1. Copy the tabpage_sort.vim script to the $HOME/.vim/plugin or
"    the $HOME/vimfiles/plugin. Refer to the ':help add-plugin',
"    ':help add-global-plugin' and ':help runtimepath' topics for
"    more details about Vim plugins.
" 2. Restart Vim.
"
" Usage
" -----
" 1. Run command ':TabpageSort', ':TabpageSort asc', ':TabpageSort desc',
"    ':TabpageSortAsc' or ':TabpageSortDesc'.
"
"    :TabpageSort           sort vim7 tab page in buffer number ascending order.
"    :TabpageSort asc       sort vim7 tab page in buffer number ascending order.
"    :TabpageSort desc      sort vim7 tab page in buffer number descending order.
"    :TabpageSortAsc        sort vim7 tab page in buffer number ascending order.
"    :TabpageSortDesc       sort vim7 tab page in buffer number descending order.
"
" Note
" ----
" 1. If current buffer size is over current 'tabpagemax' option setting,
"    'tabpagemax' option size is automatically expanded.
"    see ":help tabpagemax".
"
"
:if exists('g:tabpage_sort_loaded') || version < 700
    :finish
:endif

:let s:initial_tabpagemax = &tabpagemax

:command! -nargs=0 TabpageSortAsc  call s:TabpageSortAsc()
:command! -nargs=0 TabpageSortDesc call s:TabpageSortDesc()
:command! -nargs=? TabpageSort call s:TabpageSort(<q-args>)

:function! s:CurrentTabPositionAsc()
    :let l:current_bufnr = bufnr("%")
    :if buflisted(l:current_bufnr)
    :else
        :return 0
    :endif

    :let l:i = 0
    :let l:tab_position = 0
    :let l:max = bufnr("$")
    :while l:i <= l:max
        :let l:i = l:i + 1
        :if buflisted(l:i)
            :if l:i <= l:current_bufnr
                :let l:tab_position = l:tab_position + 1
            :else
                :break
            :endif
        :endif
    :endwhile

    :return l:tab_position
:endfunction

:function! s:CurrentTabPositionDesc()
    :let l:current_bufnr = bufnr("%")
    :if buflisted(l:current_bufnr)
    :else
        :return 0
    :endif

    :let l:tab_position = 0
    :let l:max = bufnr("$")
    :while 1 <= l:max
        :if buflisted(l:max)
            :if l:max >= l:current_bufnr
                :let l:tab_position = l:tab_position + 1
            :else
                :break
            :endif
        :endif
        :let l:max = l:max - 1
    :endwhile

    :return l:tab_position
:endfunction

:function! s:TabSize()
    :let l:i = 0
    :let l:count = 0
    :let l:max = bufnr("$")
    :while l:i <= l:max
        :let l:i = l:i + 1
        :if buflisted(l:i)
            :let l:count = l:count + 1
        :endif
    :endwhile

    :return l:count
:endfunction

:function! s:UpdateTabpagemax()
    :let l:tabsize = s:TabSize()
    :if l:tabsize > s:initial_tabpagemax
        :let &tabpagemax = l:tabsize
    :else
        :let &tabpagemax = s:initial_tabpagemax
    :endif
:endfunction

:function! s:OpenTabAsc()
    :silent tabonly

    :let l:i = 0
    :let l:max = bufnr("$")
    :while l:i <= l:max
        :let l:i = l:i + 1
        :if buflisted(l:i)
            :execute "silent tabnew " . bufname(l:i)
        :endif
    :endwhile

    :silent tabfirst
    :silent tabclose
:endfunction

:function! s:OpenTabDesc()
    :silent tabonly

    :let l:max = bufnr("$")
    :while 1 <= l:max
        :if buflisted(l:max)
            :execute "silent tabnew " . bufname(l:max)
        :endif
        :let l:max = l:max - 1
    :endwhile

    :silent tabfirst
    :silent tabclose
:endfunction

:function! s:TabpageSortAsc()
    :echo "sort tabpage order by buffer number asc."
    :let l:tab_position = s:CurrentTabPositionAsc()

    :call s:UpdateTabpagemax()
    :call s:OpenTabAsc()

    :if l:tab_position > 0
        :execute ":tabnext " . l:tab_position
    :endif
:endfunction

:function! s:TabpageSortDesc()
    :echo "sort tabpage order by buffer number desc."
    :let l:tab_position = s:CurrentTabPositionDesc()

    :call s:UpdateTabpagemax()
    :call s:OpenTabDesc()

    :if l:tab_position > 0
        :execute ":tabnext " . l:tab_position
    :endif
:endfunction

:function! s:TabpageSort(...)
    :if len(a:1) > 0
        :if a:1 =~ "^a"
            :call s:TabpageSortAsc()
        :elseif a:1 =~ "^d"
            :call s:TabpageSortDesc()
        :else
            :call s:TabpageSortAsc()
        :endif
    :else
        :call s:TabpageSortAsc()
    :endif
:endfunction

" vim:set ft=vim et :
