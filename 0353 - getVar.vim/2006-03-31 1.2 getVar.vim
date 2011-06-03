" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/getVar.vim" {{{
" LAST MODIFICATION: "Fri, 31 Mar 2006 17:25:02 "
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" Version 1.2

" Tries to return the window-specific value of a variable; if not found, tries
" to return  the buffer-specific value --  if even that's not  found, tries to
" return the global value -- if  that's not found either, returns the optional
" second parameter (-1 if it's not specified)
"
" Updated on March 31, 2006 (17:24:32) to use Vim 7's t: (tab-specific) variables once buffer-specific variables aren't
" found.  The new sequence is window -> buffer -> tab -> global.
function! GetVar( ... )
  let varName=a:1

  let retVal = exists( "a:2" ) ? a:2 : -1

  if ( exists ( "w:" . varName ) )
    let retVal=w:{varName}
  elseif ( exists ( "b:" . varName ) )
    let retVal=b:{varName}
  elseif ( exists ( "t:" . varName ) )
    let retVal=t:{varName}
  elseif ( exists ( "g:" . varName ) )
    let retVal=g:{varName}
  endif
  return retVal
endfunction

" just checks to  see if either the  window, buffer or global  version of this
" variable exists (useful in scripts that only check for existence)
"
" this can't use GetVar because the variable might actually BE -1.
function! VarExists( varName )
  return ( exists( "w:" . a:varName ) || exists( "b:" . a:varName ) || exists( "t:" . a:varName ) || exists ( "g:" . a:varName ) )
endfunction
