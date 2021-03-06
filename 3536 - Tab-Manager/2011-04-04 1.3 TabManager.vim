" TabManager.vim: a plugin to rearrange open buffers across multiple tabs in Vim.
" By: Salman Halim
" Version 1.2
" Date: Saturday, April 02, 2011
"
" Sorts and rearranges all open buffers across tabs based on built-in or user-specified criteria.
"
" Configuration: The variable g:TabManager_maxFilesInTab (defaults to 5) is used to determine how many buffers to open in each tab.
"
" General usage:
"
" All of these commands optionally take a number which determines how many buffers to open per tab (new tabs get created as necessary); 0 means to lump all
" buffers of any given typeinto one tab.
"
" For the following examples, assume these buffers are open (doesn't matter how many tabs they currently take):
"
" /progs/com/test/Abcd.java
" /progs/com/test/ProgramRunner.java
" /progs/com/abc/Test.java
" /vim/plugin/TabManager.vim
"
" Version 1.3:
"
" Rearrangetabsbyfirstletter renamed to Rearrangetabsbyfirstletters: If called with no parameters, considers the first character of the file name, as before. If
" called with one parameter, the specified number of characters is considered. For example, with no parameters, "Test.java" and "Touring.java" would be placed
" together because both start with a T. With 2 as the parameter value, they would be separated ("Te" vs. "To").
"
" If called with 2 parameters, the first parameter is the number of windows to place in one tab, as always (0 meaning to throw them all together).
"
" Version 1.2:
"
" New variable: g:TabManager_fileTypeExtension (defaults to "java") to use when determining the Rearrangetabsbytype; can be passed to the command on the
" command-line as an override.
"
" Rearrangetabsbytype now takes an extension (other than the default "java" or g:TabManager_fileTypeExtension) to use when sorting the files. If only one
" parameter is passed, it may be either a number or the extension. If both are specified, the number must be first.
"
" Version 1.1:
"
" Added two new commands:
"
" Rearrangetabsbytype: If it's a Java file, returns the last word that's not a part of the extension. For example, CopyAction.java returns Action and
" CopyForm.java returns Form. This should probably be reworked to allow the setting of the extension upon which to filter or to not look at extensions at all.
"
" Rearrangetabsbyroot: Groups files with similarly named roots together. For example, CopyAction.java, CopyForm.java and CopyResults.jsp will end up together
" because they all have "Copy" as the root.
"
" Version 1.0:
"
" Rearrangetabsbyfirstletter: Quick alphabetization of buffers where all buffers with the same starting letter are put together. Only the file name is
" considered, not the path. So, at the end, you get three tabs:
"
" Tab 1: Abcd.java
" Tab 2: ProgramRunner.java
" Tab 3: TabManager.vim and Test.java
"
" (The tabs are sorted as are the results within each tab.)
"
" Rearrangetabsbyextension: Arranges by extension (.java or .vim, for example):
"
" Tab 1: Abcd.java, ProgramRunner.java and Test.java
" Tab 2: TabManager.vim
"
" Rearrangetabsbypath: Arranges by absolute file path (not taking filename into consideration):
"
" Tab 1: /progs/com/abc: Test.java
" Tab 2: /progs/com/test: Abcd.java and ProgramRunner.java
" Tab 3: /vim/plugin: TabManager.vim
"
" Tiletabs: Quick way to alphabetize the list of buffers (case sensitive) into regularly-sized (by number of buffers) tabs. Doesn't make sense to use this with
" 0 (all buffers in one tab), unless that's the effect you want.
"
" Rearrangetabs: The workhorse. All other other commands are defined through this. Takes the actual pattern criteria against which to match each buffer.


if ( !exists( "g:TabManager_maxFilesInTab" ) )
  let g:TabManager_maxFilesInTab = 5
endif

if ( !exists( "g:TabManager_fileTypeExtension" ) )
  let g:TabManager_fileTypeExtension = "java"
endif

" If it's a Java file, returns the last word that's not a part of the extension. For example, CopyAction.java returns Action and CopyForm.java returns Form.
function! GetFileType( ... )
  let filename  = exists( "a:1" ) ? a:1 : expand( "%:t" )
  let extension = fnamemodify( filename, ":e" )
  let filename  = fnamemodify( filename, ":r" )

  if ( extension != g:TabManager_fileTypeExtension )
    return extension
  endif

  return substitute( filename, '\C^.*\([A-Z][^A-Z]*\)$', '\1', '' )
endfunction

" Groups files with similarly named roots together. For example, CopyAction.java, CopyForm.java and CopyResults.jsp will end up together because they all have
" "Copy" as the root.
function! GetFileRoot( ... )
  let filename  = exists( "a:1" ) ? a:1 : expand( "%:t" )
  let extension = fnamemodify( filename, ":e" )
  let filename  = fnamemodify( filename, ":r" )

  return substitute( filename, '\C^\(.*\)[A-Z][^A-Z]*$', '\1', '' )
endfunction

" Returns based on the expression, prepending a "." to be safe.
function! s:GetFileKeyByExpression()
  execute 'return "." . ' . g:TabManager_keyExpression
endfunction

let s:expressionFunctionReference = function( "<SID>GetFileKeyByExpression" )

function! s:CloseOldTabs()
  if ( !exists( "t:TabManager_key" ) )
    tabclose
  endif
endfunction

function! s:CollectFileInformation()
  let key = g:TabManager_keyFunction()

  if ( !has_key( g:TabManager_keyList, key ) )
    let g:TabManager_keyList[ key ] = []
  endif

  let fileInformation = {}

  let fileInformation.path     = expand( "%:p" )
  let fileInformation.position = winsaveview()

  let g:TabManager_keyList[ key ] += [ fileInformation ]
endfunction

function! s:CollectKeys()
  let g:TabManager_keyList = {}

  " Empty keys just become "."; otherwise, it throws an error about an empty key.
  tabdo windo call <SID>CollectFileInformation()

  let result = g:TabManager_keyList

  unlet g:TabManager_keyList

  return result
endfunction

function! s:RearrangeTabsByFileType( ... )
  let numFiles         = g:TabManager_maxFilesInTab
  let extensionToParse = g:TabManager_fileTypeExtension

  " At least two arguments; the first one is the number of files and the second is the extension.
  if ( a:0 >= 2 )
    let numFiles         = a:1
    let extensionToParse = a:2
  elseif ( a:0 == 1 )
    if ( str2nr( a:1 ) == a:1 )
      let numFiles = a:1
    else
      let extensionToParse = a:1
    endif
  endif

  let savedExtension                 = g:TabManager_fileTypeExtension
  let g:TabManager_fileTypeExtension = extensionToParse

  execute 'Rearrangetabs ' . numFiles . ' GetFileType()'

  let g:TabManager_fileTypeExtension = savedExtension
endfunction

function! s:RearrangeTabsByFirstLetters( ... )
  let numFiles   = g:TabManager_maxFilesInTab
  let numLetters = 1

  if ( a:0 >= 2 )
    let numFiles = a:1
    let numLetters = a:2
  elseif ( a:0 == 1 )
    let numLetters = a:1
  endif

  execute "Rearrangetabs " . numFiles . " substitute( expand( '%' ), '^\\(.\\{" . numLetters . "}\\).*', '\\1', '' )"
endfunction

" Pass it a function that can be used to generate a filtering key for the current buffer. For example, GetFileExtension returns the file extension and
" GetFilePath returns the fully qualified file path.
"
" Leaving this exposed in case a more powerful function is required than a simple expression. As long as the function returns a string value, a FuncRef to it
" can be passed in.
"
" Tip: Prepend a "." to the return value to take care of the case where the returned value is the empty string.
function! RearrangeTabs( keyFunction, ... )
  silent! tabdo unlet t:TabManager_key

  let g:TabManager_keyFunction = a:keyFunction
  let fileList                 = <SID>CollectKeys()
  let maxFiles                 = g:TabManager_maxFilesInTab

  if ( exists( "a:1" ) && a:1 != "" )
    let maxFiles = a:1
  endif

  unlet g:TabManager_keyFunction

  for key in sort( keys( fileList ) )
    let numFiles = 0

    for fileInformation in sort( fileList[ key ] )
      " Either the very first time or, after that, if paging is desired and the maximum number of items has been reached.
      if ( numFiles == 0 || ( maxFiles > 0 && numFiles % maxFiles == 0 ) )
        tabnew

        let t:TabManager_key = key
      endif

      execute 'Sp ' . fileInformation.path

      call winrestview( fileInformation.position )

      let numFiles += 1
    endfor
  endfor

  tabdo call <SID>CloseOldTabs()

  tabn 1
endfunction

let s:argumentParsingExpression = '^\%(\(\d\+\)\s\+\)\?\(.*\)$'

function! RearrangeTabsByExpression( arg )
  let parsedList = matchlist( a:arg, s:argumentParsingExpression )

  let numFiles                   = parsedList[ 1 ] == "" ? g:TabManager_maxFilesInTab : parsedList[ 1 ]
  let g:TabManager_keyExpression = parsedList[ 2 ]

  call RearrangeTabs( s:expressionFunctionReference, numFiles )
endfunction

" Allows the passing in of an expression that, when evaluated (by passing to :execute 'return "." ' . <expression>), returns a string based on the current
" buffer's parameters. The first--optional--parameter is the number of files to create per tab. Leaving it out causes the value in g:TabManager_maxFilesInTab to
" be used. To allow as many as will fit (this doesn't actually check for a maximum but will cram them in as they come), use 0.
com! -nargs=+ Rearrangetabs silent call RearrangeTabsByExpression( <q-args> )

com! -nargs=* Rearrangetabsbyfirstletters call <SID>RearrangeTabsByFirstLetters( <f-args> )
com! -nargs=? Rearrangetabsbyextension    Rearrangetabs <args> expand( "%:e" )
com! -nargs=? Rearrangetabsbypath         Rearrangetabs <args> expand( "%:p:h" )
com! -nargs=* Rearrangetabsbytype         call <SID>RearrangeTabsByFileType( <f-args> )
com! -nargs=? Rearrangetabsbyroot         Rearrangetabs <args> GetFileRoot()

" Simply tiles all tabs, ignoring any particular attributes.
com! -nargs=? Tiletabs Rearrangetabs <args> ''
