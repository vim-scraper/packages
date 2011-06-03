" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/getVar.vim" {{{
" LAST MODIFICATION: "Fri, 26 Jul 2002 09:25:11 Eastern Daylight Time"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" tries to return the buffer-specific value of a variable; if not found, tries
" to  return the  global value  --  if that's  not found  either, returns  the
" optional second parameter (-1 if it's not specified)
function! GetVar(...)
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

" just checks to  see if either the  buffer, global or script  version of this
" variable exists (useful in scripts that only check for existence)
"
" this can't use GetVar because the variable might actually be -1!
function! VarExists(varName)
  return (exists("b:" . a:varName) || exists ("g:" . a:varName))
endfunction
