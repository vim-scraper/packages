" TabManager.vim: a plugin to rearrange open buffers across multiple tabs in Vim.
" By: Salman Halim
"
" Sorts and rearranges all open buffers across tabs based on built-in or user-specified criteria.
"
" Configuration: The variable g:TabManager_maxFilesInTab (defaults to 5) is used to determine how many buffers to open in each tab.
"
" General usage:
"
" All of these commands optionally take a number which determines how many buffers to open per tab (new tabs get created as necessary); 0 means to lump all
" buffers of any given type into one tab.
"
" For the following examples, assume these buffers are open (doesn't matter how many tabs they currently take):
"
" /progs/com/test/Abcd.java
" /progs/com/test/ProgramRunner.java
" /progs/com/abc/Test.java
" /vim/plugin/TabManager.vim
"
" Version 1.55:
"
" Rearrangetabsbypath now also takes a second parameter which determines how many components from the path to consider. For example, given the following set of
" files:
"
" /progs/com/test/Abcd.java
" /progs/com/test/ProgramRunner.java
" /progs/com/abc/Test.java
"
" The default behaviour would be to create two tabs, one with Abcd.java and ProgramRunner.java; and the other with Test.java. However, if called with a
" parameter of 2, it will only consider the first two path components (/progs/com) and put all the files in the same tab. A value of 0 (the default) means to
" check the entire path, as before. New command format:
"
" Rearrangetabsbypath [number of files per tab] [number of path components to consider]
" Or
" Rearrangetabsbypath [number of path components to consider]
"
" Version 1.5:
"
" Added the ability to set custom "keys" to be used when rearranging files into tabs. For example, given the following list of files:

" /progs/com/test/Abcd.java
" /progs/com/test/ProgramRunner.java
" /progs/com/abc/Test.java
" /vim/plugin/TabManager.vim
" /webFiles/root/test.html
" /webFiles/root/css/styles.css
"
" The default behaviour, when arranging by path, extension or first letters, is to always place test.html and styles.css in separate tabs. (Unless calling
" Tiletabs, which just tiles all windows blindly.) However, if both these files have a manually set key of, say, "web", then any other criteria will be ignored
" and these two files will always be placed together because they have the same key. This allows grouping some windows by functionality while continuing to
" group others by whatever rearrange command is actually called.
"
" Of course, different sets of windows may have different keys and they will all be grouped together accordingly.
"
" All of the standard Rearrange* commands now take an optional bang (!)--for example, Rearrangetabsbyextension!--that will, for just this rearrangement, IGNORE
" any local keys and treat all windows as if there were no keys.
"
" If you have multiple windows where you're editing the same buffer, they will almost always end up together after any rearrangement (unless they are split
" across tabs that become full). You can avoid this by setting different window-level local keys for the buffers.
"
" Added four new commands--the set and remove key commands can be combined with windo and tabdo to set or remove the same key for more than one window at a time:
"
" Settabmanagerwindowkey <key>: Sets the window-level local key for the current window to <key>.
"
" Settabmanagerbufferkey <key>: Sets the buffer-level local key for the current buffer to <key>.
"
" Removetabmanagerkey: Removes the currently set key for the current window; will remove BOTH buffer and window level keys. (Doesn't take any arguments.)
"
" Redorearrangetabs: Redoes the last tab rearrangement command (by extension, simple tiling, by first letters, etc.). Useful for incorporating new windows into
" the currenty "scheme". If local keys have changed, the layout may end up being different. If new windows have been added, the layout may be different, also,
" but that may be what you want. By default, it obeys local keys (even if the last command was called with a bang and did not). Accepts a bang (!) to ignore
" local keys.
"
" If you call this before you've run any actual rearrangement commands, you will most likely (deservedly) get an error.
"
" Version 1.45:
"
" Added new command:
"
" Copytotab: copies the current file to the specified tab (just like Movetotab except that the original tab also contains the file).
"
" Version 1.4:
"
" Added new command:
"
" Movetotab: moves the current file to the specified tab; understands absolute numbers (1 for the first tab, for example) as well as:
"
" - n: next tab
" - p: previous tab
" - $: last tab
"
" Version 1.35:
"
" Takes 'ignorecase' into consideration for matching extensions, first letters and complete file paths (but not for matching roots and "types").
"
" Version 1.3:
"
" Rearrangetabsbyfirstletter renamed to Rearrangetabsbyfirstletters: If called with no parameters, considers the first character of the file name, as before.
"
" If called with one parameter, the specified number of characters is considered. For example, with no parameters, "Test.java" and "Touring.java" would be
" placed together because both start with a T. With 2 as the parameter value, they would be separated ("Te" vs. "To").
"
" If called with two parameters, the first parameter is the number of windows to place in one tab, as always (0 meaning to throw them all together).
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
  execute 'return ' . g:TabManager_keyExpression
endfunction

let s:expressionFunctionReference = function( "<SID>GetFileKeyByExpression" )

function! s:CloseOldTabs()
  if ( !exists( "t:TabManager_key" ) )
    tabclose
  endif
endfunction

function! GetFileStats()
  let fileInformation = {}

  let fileInformation.path      = expand( "%:p" )
  let fileInformation.position  = winsaveview()

  " If the window key exists, save it so we can restore it.
  if ( exists( "w:TabManager_localKey" ) )
    let fileInformation.windowKey = w:TabManager_localKey
  endif

  return fileInformation
endfunction

function! s:CollectFileInformation()
  let key = '.'

  if ( g:TabManager_bang )
    let key .= g:TabManager_keyFunction()
  else
    let key .= GetVar( 'TabManager_localKey', g:TabManager_keyFunction() )
  endif

  if ( !has_key( g:TabManager_keyList, key ) )
    let g:TabManager_keyList[ key ] = []
  endif

  let g:TabManager_keyList[ key ] += [ GetFileStats() ]
endfunction

function! s:CollectKeys()
  let g:TabManager_keyList = {}

  " Empty keys just become "."; otherwise, it throws an error about an empty key.
  tabdo windo call <SID>CollectFileInformation()

  let result = g:TabManager_keyList

  unlet g:TabManager_keyList

  return result
endfunction

function! GetFilePath( numPathComponents )
  let path = &ignorecase ? tolower( expand( "%:p:h" ) ) : expand( "%:p:h" )
  let path = substitute( path, '\', '/', 'g' )

  if ( a:numPathComponents == 0 )
    return path
  else
    return substitute( path =~ '/$' ? path : path . '/', '^\(\%([^\/]*\/\)\{,' . a:numPathComponents . '}\).*', '\1', '' )
  endif
endfunction

function! s:RearrangeTabsByPath( bang, ... )
  let numFiles          = g:TabManager_maxFilesInTab
  let numPathComponents = 0

  " At least two arguments; the first one is the number of files and the second is the maximum number of path components to consider.
  if ( a:0 >= 2 )
    let numFiles          = a:1
    let numPathComponents = a:2
  elseif ( a:0 == 1 )
    let numPathComponents = a:1
  endif

  execute 'Rearrangetabs' . a:bang . ' ' . numFiles . ' GetFilePath( ' . numPathComponents . ' )'
endfunction

function! s:RearrangeTabsByFileType( bang, ... )
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

  execute 'Rearrangetabs' . a:bang . ' ' . numFiles . ' GetFileType()'

  let g:TabManager_fileTypeExtension = savedExtension
endfunction

function! s:RearrangeTabsByFirstLetters( bang, ... )
  let numFiles   = g:TabManager_maxFilesInTab
  let numLetters = 1

  if ( a:0 >= 2 )
    let numFiles = a:1
    let numLetters = a:2
  elseif ( a:0 == 1 )
    let numLetters = a:1
  endif

  execute "Rearrangetabs" . a:bang. " " . numFiles . " substitute( &ignorecase ? tolower( expand( '%:t' ) ) : expand( '%:t' ), '^\\(.\\{" . numLetters . "}\\).*', '\\1', '' )"
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

        execute 'e ' . fileInformation.path
      else
        execute 'sp ' . fileInformation.path
      endif

      call winrestview( fileInformation.position )

      " Restore the local window key--creating a new window and moving a buffer over destroys it.
      if ( has_key( fileInformation, 'windowKey' ) )
        let w:TabManager_localKey = fileInformation.windowKey
      endif

      let numFiles += 1
    endfor
  endfor

  tabdo call <SID>CloseOldTabs()

  tabn 1
endfunction

" Moves the current buffer to the specified tab; if a second parameter is specified and is 1, the behaviour is that of a copy (the current buffer isn't closed
" but another copy is created).
function! MoveToTab( desiredTab, ... )
  let savedLazy = &lz

  set lazyredraw

  let currentTab = tabpagenr()
  let tabNumber  = a:desiredTab

  if ( tabNumber == 'n' )
    let tabNumber = currentTab + 1
  elseif ( tabNumber == 'p' )
    let tabNumber = currentTab - 1
  elseif ( tabNumber == '$' )
    let tabNumber = tabpagenr( '$' )
  endif

  if ( currentTab == tabNumber )
    echo "Already on tab " . tabNumber

    return
  elseif ( tabpagenr( '$' ) < tabNumber )
    echo "No such tab."

    return
  endif

  let fileInformation = GetFileStats()

  execute 'tabn ' . tabNumber

  execute 'sp ' . fileInformation.path

  " Close current; must do this after opening it elsewhere in case it's been modified.
  if ( !exists( "a:1" ) || a:1 != "1" )
    execute 'tabn ' . currentTab
    execute 'q'

    execute 'tabn ' . tabNumber
  endif

  execute savedLazy

  " Have to do this last.
  call winrestview( fileInformation.position )
endfunction

let s:argumentParsingExpression = '^\%(\(\d\+\)\s\+\)\?\(.*\)$'

function! RearrangeTabsByExpression( bang, arg )
  let parsedList = matchlist( a:arg, s:argumentParsingExpression )

  let g:TabManager_numFiles      = parsedList[ 1 ] == "" ? g:TabManager_maxFilesInTab : parsedList[ 1 ]
  let g:TabManager_keyExpression = parsedList[ 2 ]
  let g:TabManager_bang          = a:bang == '!'

  call RearrangeTabs( s:expressionFunctionReference, g:TabManager_numFiles )
endfunction

" Allows the passing in of an expression that, when evaluated (by passing to :execute 'return "." ' . <expression>), returns a string based on the current
" buffer's parameters. The first--optional--parameter is the number of files to create per tab. Leaving it out causes the value in g:TabManager_maxFilesInTab to
" be used. To allow as many as will fit (this doesn't actually check for a maximum but will cram them in as they come), use 0.
com! -nargs=+ -bang Rearrangetabs silent call RearrangeTabsByExpression( <q-bang>, <q-args> )

com! -nargs=* -bang Rearrangetabsbyfirstletters call <SID>RearrangeTabsByFirstLetters( <q-bang>, <f-args> )
com! -nargs=? -bang Rearrangetabsbyextension    Rearrangetabs<bang> <args> &ignorecase ? tolower( expand( "%:e" ) ) : expand( "%:e" )
com! -nargs=* -bang Rearrangetabsbypath         call <SID>RearrangeTabsByPath( <q-bang>, <f-args> )
com! -nargs=* -bang Rearrangetabsbytype         call <SID>RearrangeTabsByFileType( <q-bang>, <f-args> )
com! -nargs=? -bang Rearrangetabsbyroot         Rearrangetabs<bang> <args> GetFileRoot()

com! -nargs=1 Settabmanagerwindowkey let w:TabManager_localKey = "<args>"
com! -nargs=1 Settabmanagerbufferkey let b:TabManager_localKey = "<args>"
com! Removetabmanagerkey silent! unlet w:TabManager_localKey b:TabManager_localKey

" Redoes the last rearrange command.
com! -bang Redorearrangetabs execute 'Rearrangetabs<bang> ' . g:TabManager_numFiles . ' ' . g:TabManager_keyExpression

" Simply tiles all tabs, ignoring any particular attributes; call with a bang (!) to ignore local keys, also.
com! -nargs=? -bang Tiletabs Rearrangetabs<bang> <args> ''

com! -nargs=1 Movetotab call MoveToTab( <q-args> )
com! -nargs=1 Copytotab call MoveToTab( <q-args>, 1 )
