:if &cp || exists("g:loaded_catn")
    :finish
:endif
:let g:loaded_catn = 1

:let s:save_cpo = &cpo
:set cpo&vim

" judge argument count
:function! s:Catn(...) range
    :if len(a:000) == 0
        :call s:CatnFormat("%d ", 1, a:firstline, a:lastline)
    :elseif len(a:000) == 1
        :call s:CatnFormat("%d ", a:1, a:firstline, a:lastline)
    :else
        :call s:CatnFormat(a:1, a:2, a:firstline, a:lastline)
    :endif
:endfunction

" insert formated text at left end
:function! s:CatnFormat(format, start_no, start, end)
    " loop and insert
    :let l:i = 0
    :while (a:start + l:i) <= a:end
        " formated text
        :let l:no_fmt = printf(a:format, a:start_no + l:i)
        " add text
        :let l:line_fmt = printf("%s%s", l:no_fmt, getline(a:start + l:i))
        :call setline(a:start + l:i, l:line_fmt)
        :let l:i += 1
    :endwhile
:endfunction

" command
:command! -narg=* -range Catn :<line1>,<line2>call s:Catn(<f-args>)

:let &cpo = s:save_cpo
:finish

==============================================================================
catn.vim : insert formatted numbering text.
------------------------------------------------------------------------------
$VIMRUNTIMEPATH/plugin/catn.vim
==============================================================================
author  : OMI TAKU
url     : http://nanasi.jp/
email   : mail@nanasi.jp
version : 2008/04/25 15:00:00
==============================================================================

Insert formatted text with number at selected area.
You can select "text format", "starting number", and "insert area".

Default value of option are
text format     : "%d "
starting number : 1
insert area     : current line only

------------------------------------------------------------------------------

[command format]

:'<,'>Catn {text format} {starting number}

:'<,'>Catn [{starting number}]

({text format} ... see ":help printf()", available format is written there.)

------------------------------------------------------------------------------

[command example]

(Insert number at selected area. 1, 2, 3, ....)
:'<,'>Catn

(Insert number at selected area. 1000, 1001, 1002, ....)
:'<,'>Catn 1000

(Insert formatted number text at selected area. 0000500, 0000501, 0000502, ....)
:'<,'>Catn %08d\  500

==============================================================================

1. Copy the catn.vim script to
   $HOME/vimfiles/plugin or $HOME/.vim/plugin directory.
   Refer to ':help add-plugin', ':help add-global-plugin' and
   ':help runtimepath' for more details about Vim plugins.

2. Restart Vim.

==============================================================================
" vim: set ff=unix et ft=vim nowrap :
