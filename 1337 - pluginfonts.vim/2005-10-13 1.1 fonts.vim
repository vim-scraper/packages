"-------------------------------------------------------------------------------

:if has ("vms")
    :let g:Font_L1="-dec-terminal-medium-r-normal-*-14-*-100-100-c-*-iso8859-1"
    :let g:Font_L2="-misc-fixed-medium-r-normal-*-17-*-100-100-c-*-iso8859-1"
    :let g:Font_L3="-bitstream-terminal-medium-r-normal-*-18-*-100-100-c-*-iso8859-1"
    :let g:Font_U1="-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1"
    :let g:Font_U2="-xos4-terminus-medium-r-normal-*-17-*-*-*-c-*-iso10646-1"
    :let g:Font_U3="-xos4-terminus-medium-r-normal-*-20-*-*-*-c-*-iso10646-1"
:elseif has ("gui_win32")
    :let g:Font_L1="Bitstream_Vera_Sans_Mono:h9:cANSI"
    :let g:Font_L2="Bitstream_Vera_Sans_Mono:h11:cANSI"
    :let g:Font_L3="Bitstream_Vera_Sans_Mono:h13:cANSI"
    :let g:Font_U1="Bitstream_Vera_Sans_Mono:h9:cDEFAULT"
    :let g:Font_U2="Bitstream_Vera_Sans_Mono:h11:cDEFAULT"
    :let g:Font_U3="Bitstream_Vera_Sans_Mono:h13:cDEFAULT"
:elseif has ("gui_gtk")
    :let g:Font_L1="Bitstream Vera Sans Mono 9"
    :let g:Font_L2="Bitstream Vera Sans Mono 11"    
    :let g:Font_L3="Bitstream Vera Sans Mono 13"
    :let g:Font_U1=g:Font_L1
    :let g:Font_U2=g:Font_L2   
    :let g:Font_U3=g:Font_L3
:elseif has ("gui_kde")
    :let g:Font_L1="Bitstream Vera Sans/9/-1/5/50/0/0/"
    :let g:Font_L2="Bitstream Vera Sans/11/-1/5/50/0/0/"
    :let g:Font_L3="Bitstream Vera Sans/13/-1/5/50/0/0/"
    :let g:Font_U1=g:Font_L1
    :let g:Font_U2=g:Font_L2   
    :let g:Font_U3=g:Font_L3
:else
    :let g:Font_L1="-bitstream-bitstream vera sans mono-medium-r-normal-*-14-*-100-100-m-*-iso8859-1"
    :let g:Font_L2="-bitstream-bitstream vera sans mono-medium-r-normal-*-16-*-100-100-m-*-iso8859-1"
    :let g:Font_L3="-bitstream-bitstream vera sans mono-medium-r-normal-*-18-*-100-100-m-*-iso8859-1"
    :let g:Font_U1="-bitstream-bitstream vera sans mono-medium-r-normal-*-14-*-100-100-m-*-iso10646-1"
    :let g:Font_U2="-bitstream-bitstream vera sans mono-medium-r-normal-*-16-*-100-100-m-*-iso10646-1"
    :let g:Font_U3="-bitstream-bitstream vera sans mono-medium-r-normal-*-18-*-100-100-m-*-iso10646-1"
:endif

:function! <SID>Set_Font (Size)
    :if a:Size != 0
        :let g:Font_Size=a:Size
    :end
    :if &encoding == "utf-8"
        :let &guifont=g:Font_U{g:Font_Size}        
    :else
        :let &guifont=g:Font_L{g:Font_Size}        
    :endif        
:endfunction

:if &diff
    :call <SID>Set_Font (1)
:else
    :call <SID>Set_Font (2)
:endif

:autocmd EncodingChanged * :call <SID>Set_Font (0)

:command! Font1  :call <SID>Set_Font (1)
:command! Font2  :call <SID>Set_Font (2)
:command! Font3  :call <SID>Set_Font (3)

:nnoremap <silent> <Leader>1 :Font1<CR>
:nnoremap <silent> <Leader>2 :Font2<CR>
:nnoremap <silent> <Leader>3 :Font3<CR>

:48menu <silent> Plugin.Font.Small<Tab>\\1  :Font1<CR>
:48menu <silent> Plugin.Font.Medium<Tab>\\2 :Font2<CR>
:48menu <silent> Plugin.Font.Large<Tab>\\3  :Font3<CR>

"-------------------------------------------------------------------------------
" vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab
" vim: filetype=vim encoding=latin1 fileformat=unix
