" -*- vim -*-
" FILE: "C:/vim/Vimfiles/plugin/getVar.vim" {{{
" LAST MODIFICATION: "Mon, 22 Sep 2003 09:01:40 Eastern Daylight Time (Administrator)"
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" Version 1.1

" Tries to return the window-specific value of a variable; if not found, tries
" to return  the buffer-specific value --  if even that's not  found, tries to
" return the global value -- if  that's not found either, returns the optional
" second parameter (-1 if it's not specified)
function! GetVar(...)
  let varName=a:1

  if (exists("a:2"))
    let retVal=a:2
  else
    let retVal=-1
  endif

  if (exists ("w:" . varName))
    let retVal=w:{varName}
  elseif (exists ("b:" . varName))
    let retVal=b:{varName}
  elseif (exists ("g:" . varName))
    let retVal=g:{varName}
  endif
  return retVal
endfunction

" just checks to  see if either the  window, buffer or global  version of this
" variable exists (useful in scripts that only check for existence)
"
" this can't use GetVar because the variable might actually BE -1.
function! VarExists(varName)
  return (exists("w:" . a:varName) || exists("b:" . a:varName) || exists ("g:" . a:varName))
endfunction
