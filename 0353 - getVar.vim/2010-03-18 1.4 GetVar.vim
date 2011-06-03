" -*- vim -*-
" FILE: "C:/vim/Vimfiles/plugin/GetVar.vim" {{{
" LAST MODIFICATION: "Fri, 31 Mar 2006 17:25:02 "
" (C) 2002 by Salman Halim, <salmanhalim@hotmail.com>
" $Id:$ }}}

" Version 1.4
"
" Added two new functions: GetSafe and Allocate.
"
" Version 1.3
"
" Changed the plugin slightly to allow for a default value to continue working if the regular return value is a list (or dictionary; basically, not a simple
" scalar value). The change, while bringing the plugin up to scratch with Vim 7.x, doesn't break Vim 6.x compatibility.
"
" Version 1.2
"
" Tries to return the window-specific value of a variable; if not found, tries
" to return  the buffer-specific value --  if even that's not  found, tries to
" return the global value -- if  that's not found either, returns the optional
" second parameter (-1 if it's not specified)
"
" Updated on March 31, 2006 (17:24:32) to use Vim 7's t: (tab-specific) variables once buffer-specific variables aren't
" found.  The new sequence is window -> buffer -> tab -> global.
function! GetVar( ... )
  let varName=a:1

  if ( exists ( "w:" . varName ) )
    let retVal=w:{varName}
  elseif ( exists ( "b:" . varName ) )
    let retVal=b:{varName}
  elseif ( exists ( "t:" . varName ) )
    let retVal=t:{varName}
  elseif ( exists ( "g:" . varName ) )
    let retVal=g:{varName}
  else
    let retVal = exists( "a:2" ) ? a:2 : -1
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

" If the specified variable exists (passed in as a string, such as "b:someVar"), its value is passed back; otherwise, the safe value is returned instead.
function! GetSafe( varName, safeValue )
  return exists( a:varName ) ? {a:varName} : a:safeValue
endfunction

" If the specified variable does NOT exist, it is created and assigned the value specified as the defaultValue; if it does exist, nothing happens.
function! Allocate( variableName, defaultValue )
  if ( !exists( a:variableName ) )
    let {a:variableName} = a:defaultValue
  endif
endfunction
