" -*- vim -*-
" FILE: "C:/vim/Vimfiles/ftplugin/java/javabean.vim" {{{
" LAST MODIFICATION: "Wed, 15 Feb 2006 13:47:44 Eastern Standard Time"
" (C) 2001 by Salman Halim, <salman@hp.com>
" $Id:$ }}}

" given  a number  of lines  containing datatype  and member  name, this  will
" generate property getters and setters for them.  for example:
"
" String myString
" int myInt
"
" will result in:
"
" protected String m_myString;
" protected int m_myInt;
"
" public void setMyString( String val )
" {
"   m_myString = val;
" }
"
" public String getMyString()
" {
"   return m_myString;
" }
"
" public void setMyInt( int val )
" {
"   m_myInt = val;
" }
"
" public int getMyInt()
" {
"   return m_myInt;
" }
"
" should be called on a number of lines (use the mappings at the bottom of the
" file or use the command Fixproperty)  that are selected visually -- can also
" be called to act upon the current line without a visual selection.

" this can be  set on a per-buffer basis (b:javabean_scope)  to change this to
" something else (such as 'protected transient' or some such)
" let g:javabean_scope      = 'private'
" let g:javabean_beanPrefix = 'm_'
"
" Version 2.0:
"
" Added getters and setters for array variables; for example:
"
" String[] names
"
" results in:
"
" protected String[] m_names;
"
" public void setNames( String[] val )
" {
"     m_names = val;
" }
"
" public String[] getNames()
" {
"     return m_names;
" }
"
" public void setNames( String val, int index )
" {
"     m_names[ index ] = val;
" }
"
" public String getNames( int index )
" {
"     return m_names[ index ];
" }

let g:javabean_scope      = 'protected'
let g:javabean_beanPrefix = 'm_'

" the pattern that the property line is expected to be in (<type> <name> where
" <name> does NOT start with m_)
let s:linePattern = '\s*\(\S*\)\s\+\(\S*\)'

" reads the variable type and name from the current line
function! s:SetupVars()
  let s:varType = substitute(getline('.'), s:linePattern, '\1', '')
  let s:varName = substitute(getline('.'), s:linePattern, '\2', '')
  let s:capName = substitute(s:varName, '.*', '\u&', '')
  let s:varName = g:javabean_beanPrefix . s:varName
endfunction

function! s:GetVarDec()
  return s:javabean_scope . ' ' . s:varType . ' ' . s:varName . ';'
endfunction

function! s:GetSetterAndGetters()
  let getterName = s:varType ==# "boolean" ? "is" : "get"

  let setter = "public void set" . s:capName . "( " . s:varType . " val )\<CR>" . "{\<CR>" . s:varName . " = val;\<CR>}\<CR>"
  let getter = "public " . s:varType . " " . getterName . s:capName . "()\<CR>{\<CR>return " . s:varName . ";\<CR>}\<CR>"

  let result = setter . "\<CR>" . getter

  " Array variable; generate index-based getters and setters also.
  if ( s:varType =~ '\[\]$' )
    let baseType    = substitute( s:varType, '\[\]$', '', '' )
    let getterName  = baseType ==# "boolean" ? "is" : "get"
    let setterIndex = "public void set" . s:capName . "( " . baseType . " val, int index )\<CR>" . "{\<CR>" . s:varName . "[ index ] = val;\<CR>}\<CR>"
    let getterIndex = "public " . baseType . " " . getterName . s:capName . "( int index )\<CR>{\<CR>return " . s:varName . "[ index ];\<CR>}\<CR>"

    let result = result . "\<CR>" . setterIndex . "\<CR>" . getterIndex
  endif

  return result
endfunction

function! s:FixProperty() range
  let numLines = a:lastline - a:firstline + 1

  " add a spacer line between the member declarations and the method bodies
  call append(a:lastline, '')

  let i = 0
  while (i < numLines)
    " change  the current  line to  the  member declaration  by appending  the
    " declaration and removing the original line
    let currLine = i + a:firstline
    exe currLine

    " read the variable name and type before replacing the line
    call <SID>SetupVars()
    call append(currLine, <SID>GetVarDec())
    exe currLine . 'd'

    " skip ahead to the bottom part and  add the method bodies (for the setter
    " and getter) --  there wasn't a line here originally  so there is nothing
    " to remove
    let currLine = i + a:lastline + 1
    exe currLine
    call append(currLine, <SID>GetSetterAndGetters())

    let i = i + 1
  endwhile

  let fl = a:lastline + 2
  let ll = fl + numLines - 1

  exe fl . "," . ll . "s/\<CR>/\<CR>/g"

  " indent the lines to make sure they look okay
  exe a:firstline . ",']normal =="
endfunction

function! s:FixPropertyRange(line1, line2)
  if (exists ("b:javabean_scope"))
    let s:javabean_scope = b:javabean_scope
  else
    let s:javabean_scope = g:javabean_scope
  endif

  exe a:line1 . ',' . a:line2 . "call <SID>FixProperty()"
endfunction

com! -range Fixproperty silent call <SID>FixPropertyRange(<line1>, <line2>)

noremap <PLUG>Fixproperty :Fixproperty<CR>

if (!hasmapto('<PLUG>Fixproperty', 'n'))
  nmap <Leader>xp <PLUG>Fixproperty
endif

if (!hasmapto('<PLUG>Fixproperty', 'v'))
  vmap <Leader>xp <PLUG>Fixproperty
endif
