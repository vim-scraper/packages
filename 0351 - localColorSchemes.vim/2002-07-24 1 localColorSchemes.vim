" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/localColorSchemes.vim" {{{
" LAST MODIFICATION: "Wed, 24 Jul 2002 09:19:34 Eastern Daylight Time"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

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
  let scheme = s:GetVar("colorscheme", g:colors_name)

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

" tries to return the buffer-specific value of a variable; if not found, tries
" to  return the  global value  --  if that's  not found  either, returns  the
" optional second parameter (-1 if it's not specified)
function! <SID>GetVar(...)
  let varName=a:1

  if (exists("a:2"))
    let retVal=a:2
  else
    let retVal=-1
  endif

  if (exists ("b:" . varName))
    let retVal=b:{varName}
  elseif (exists ("g:" . varName))
    let retVal=g:{varName}
  endif
  return retVal
endfunction

com! -nargs=? Setlocalcolors call s:SetLocalColors(<q-args>)
com! Removelocalcolors call s:RemoveLocalColors()

" change the colorscheme and update the global colorscheme variable
com! -nargs=1 Colorscheme colorscheme <args> | let g:colorscheme = g:colors_name

map <leader>sl :execute "Setlocalcolors " . input("Enter local colorscheme [" . (exists("b:colorscheme") ? b:colorscheme : "NONE") . "]:  ")<cr>
map <leader>sg :execute "Colorscheme " . input("Enter global colorscheme [" . (exists("g:colorscheme") ? g:colorscheme : "NONE") . "]:  ")<cr>
map <leader>rl :Removelocalcolors<cr>
