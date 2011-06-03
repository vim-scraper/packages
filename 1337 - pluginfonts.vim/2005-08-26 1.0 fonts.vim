"-------------------------------------------------------------------------------

:if has ("vms")
    :let g:Font_1="-dec-terminal-medium-r-normal-*-14-*-100-100-c-*-iso8859-1"
    :let g:Font_2="-misc-fixed-medium-r-normal-*-17-*-100-100-c-*-iso8859-1"
    :let g:Font_3="-bitstream-terminal-medium-r-normal-*-18-*-100-100-c-*-iso8859-1"
:elseif has ("gui_win32")
    :let g:Font_1="Bitstream_Vera_Sans_Mono:h9:cANSI"
    :let g:Font_2="Bitstream_Vera_Sans_Mono:h11:cANSI"
    :let g:Font_3="Bitstream_Vera_Sans_Mono:h13:cANSI"
:elseif has ("gui_gtk")
    :let g:Font_1="Bitstream Vera Sans Mono 9"
    :let g:Font_2="Bitstream Vera Sans Mono 10"    
    :let g:Font_3="Bitstream Vera Sans Mono 11"
:elseif has ("gui_kde")
    :let g:Font_1="Bitstream Vera Sans/9/-1/5/50/0/0/"
    :let g:Font_2="Bitstream Vera Sans/11/-1/5/50/0/0/"
    :let g:Font_3="Bitstream Vera Sans/13/-1/5/50/0/0/"
:else
    :let g:Font_1="-bitstream-terminal-medium-r-normal-*-14-*-100-100-*-*-iso8859-1"
    :let g:Font_2="-bitstream-terminal-medium-r-normal-*-16-*-100-100-*-*-iso8859-1"
    :let g:Font_3="-bitstream-terminal-medium-r-normal-*-18-*-100-100-*-*-iso8859-1"
:endif

:if &diff
    :let &guifont=g:Font_1
:else
    :let &guifont=g:Font_2
:endif

:command! Font1  :let &guifont=g:Font_1
:command! Font2  :let &guifont=g:Font_2
:command! Font3  :let &guifont=g:Font_3

:nnoremap <silent> <Leader>1 :Font1<CR>
:nnoremap <silent> <Leader>2 :Font2<CR>
:nnoremap <silent> <Leader>3 :Font3<CR>

:48menu <silent> Plugin.Font.Small<Tab>\\1  :Font1<CR>
:48menu <silent> Plugin.Font.Medium<Tab>\\2 :Font2<CR>
:48menu <silent> Plugin.Font.Large<Tab>\\3  :Font3<CR>
        
" vim: textwidth=0 tabstop=8 shiftwidth=4 softtabstop=4 expandtab:
