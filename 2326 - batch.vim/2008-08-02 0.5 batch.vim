:if &cp || exists("g:loaded_batch")
    :finish
:endif
:let g:loaded_batch = 1

:let s:save_cpo = &cpo
:set cpo&vim

:function! s:Batch() range
    " read vimscript from selected area.
    :let l:selected = getline(a:firstline, a:lastline)
    " get temp file.
    :let l:tempfile = tempname()
    " try-finally
    :try
        " write vimscript to temp file.
        :call writefile(l:selected, l:tempfile)
        :try
            " execute temp file.
            :execute ":source " . l:tempfile
        :catch
            " catch exception
            :echohl WarningMsg |
                \ :echo "EXCEPTION :" v:exception |
                \ :echo "THROWPOINT:" v:throwpoint |
                \ :echohl None
        :endtry
    :finally
        " delete temp file.
        :if filewritable(l:tempfile)
            :call delete(l:tempfile)
        :endif
    :endtry
:endfunction

" command
:command! -range -narg=0 Batch :<line1>,<line2>call s:Batch()

:let &cpo = s:save_cpo
:finish

==============================================================================
batch.vim : execute vim script in selected area.
------------------------------------------------------------------------------
$VIMRUNTIMEPATH/plugin/batch.vim
==============================================================================
author  : OMI TAKU
url     : http://nanasi.jp/
email   : mail@nanasi.jp
version : 2008/04/25 15:00:00
==============================================================================

Execute vim script in selected area (with Visual Mode).
Plugin copy selected area text to a temporary file,
and execute temporary file as vim script file.

This plugin will be used for
    testing vim script file,
    debuging vim script,
    or loading selected vim editor options,,,,

------------------------------------------------------------------------------

[command format]

:'<,'>Batch

==============================================================================

1. Copy the batch.vim script to
   $HOME/vimfiles/plugin or $HOME/.vim/plugin directory.
   Refer to ':help add-plugin', ':help add-global-plugin' and
   ':help runtimepath' for more details about Vim plugins.

2. Restart Vim.

==============================================================================
" vim: set ff=unix et ft=vim nowrap :
