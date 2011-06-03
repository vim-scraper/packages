" -*- vim -*-
" FILE: "C:/vim/Vimfiles/plugin/localColorSchemes.vim" {{{
" LAST MODIFICATION: "Mon, 22 Sep 2003 09:03:21 Eastern Daylight Time (Administrator)"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" Version 1.1

" usage:
" either  call Setlocalcolors  from  a  particular buffer  or  put inside  the
" ftplugin file to affect all buffers of a particular filetype.
"
" call Removelocalcolors  to unlet the local  variable if it exists  (can just
" unlet the local variable manually but Remove will change the color scheme if
" necessary).

" let the  default color  scheme be whatever  is currently in  use if  none is
" explicitly specified
if (!exists("g:colorscheme"))
  let g:colorscheme = g:colors_name
endif

augroup localColors
au BufEnter * call s:ChangeColors()
augroup END

" if the  variable 'b:colorscheme'  or 'g:colorscheme'  exists it  becomes the
" color of the currently entered window
"
" the  default is  to NOT  change the  color scheme  (if the  variable doesn't
" exist)
function! <SID>ChangeColors()
  let scheme = GetVar("colorscheme", g:colors_name)

  " only change colors if necessary
  if (scheme != g:colors_name)
    execute "colorscheme " . scheme
  endif
endfunction

" if the  input parameter is  empty, display  the current local  color scheme.
" otherwise, set the local color scheme to what is specified and change colors
" is necessary.
function! <SID>SetLocalColors(colorChoice)
  if (a:colorChoice == "")
    echo "Local color scheme is:  " . (exists("b:colorscheme") ? b:colorscheme : "NONE")
  else
    let b:colorscheme = a:colorChoice
    call s:ChangeColors()
  endif
endfunction

function! <SID>RemoveLocalColors()
  " suppress the error message
  silent! execute "unlet b:colorscheme"
  call s:ChangeColors()
endfunction

com! -nargs=? Setlocalcolors call s:SetLocalColors(<q-args>)
com! Removelocalcolors call s:RemoveLocalColors()

" change the colorscheme and update the global colorscheme variable
com! -nargs=1 Colorscheme colorscheme <args> | let g:colorscheme = g:colors_name

nmap <leader>sl :execute "Setlocalcolors " . input("Enter local colorscheme [" . (exists("b:colorscheme") ? b:colorscheme : "NONE") . "]:  ")<cr>
nmap <leader>sg :execute "Colorscheme " . input("Enter global colorscheme [" . (exists("g:colorscheme") ? g:colorscheme : "NONE") . "]:  ")<cr>
nmap <leader>rl :Removelocalcolors<cr>
