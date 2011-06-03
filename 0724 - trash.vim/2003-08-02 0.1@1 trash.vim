" ==============================================================================
" ORIGINAL FILE: C:\WINDOWS\Desktop\trash.vim
" WEB: http://www.vim.org/scripts/script.php?script_id=724
" CONTENT: script for trashing dd d, D, <Del> actions erasing--- version: 0.1
" AUTHOR: George Nicolaie Nãstasie 
" HOSTNAME: PC CAGONLALACHE
" Created at: Fri 11 Jul 2003 -- 04:07:41
" ==============================================================================

" --
" Location of the stored archives: ~/vim/trash -- You must creat in your vim directory a trash directory with the name "trash"
"
" Command: Trashon   - start trashing -- default on
" Command: Trashoff  - stop trashing -- recommended for encrypted files
" Command: Trashview - view the trash file -- as a readonly file
" -- 


:if exists("trashing_on")
        :finish
:endif
:let trashing_on = 1

"trash{{{
fun! Trash()

:if bufname("") == ""
:echo "The current buffer has no name -- trashing off!"

:elseif bufname("") == bufname("")
        :let dtextbegin="Trashed at: "
        :let dtimp="".strftime("%d/%m/%y - %H:%M:%S")." "
        :let dtext=@@
        :let dlocatie="from file: ".expand("%:p")." {{{"
        :let dtextend="<<<<< end deleted text }}}"

        :let @t="".dtextbegin.dtimp.dlocatie."\n\n".dtext."\n"."\n".dtextend."\n"

        :let dir = getcwd()
                :if dir != expand("%:p:h")
                        :cd %:h
                :endif
        :sp ~/vim/trash/%.trash
        :normal gg
        :normal O
        :normal "tP
        :silent w %
        :hide
:endif
:endfunction
"}}}

"trash on{{{
:fun! Trashon()
        :nnoremap <silent> dd dd:call Trash()<cr>
        :vnoremap <silent> d d:call Trash()<cr>
        :vnoremap <silent> D D:call Trash()<cr>
        :vnoremap <silent> <Del> <Del>:call Trash()<cr>
:endfunction
"}}}

"trash off{{{
:fun! Trashoff()
        :unmap dd
        :vunmap d
        :vunmap D
        :vunmap <Del>
:endfunction
"}}}

"trash view{{{
:fun! Trashview()

:if bufname("") == ""
:echo "Nothing to view -- trashing off!"

:elseif bufname("") == bufname("")
:let dir = getcwd()
                :if dir != expand("%:p:h")
                        :cd %:h
                :endif
        :sv ~/vim/trash/%.trash
:endif
:endfunction
"}}}

"trash syntax{{{
:au! BufRead,BufEnter *.trash :syn match  WarningMsg /column/ | :syn match  WarningMsg /from file:/ | :syn match  Identifier /<<<<< end deleted text/ | :syn match  Identifier /Trashed at:/ | :syn keyword  Ignore {{{ }}}
"}}}

:command Trashon :call Trashon()
:command Trashoff :call Trashoff()
:command Trashview :call Trashview()
":command Trashdel :call delete("~/vim/trash/%.trash")
"
"
"default for trashing on
:call Trashon()
