" foldutil.vim -- utilities for creating folds.
" Author: Hari Krishna Dara (hari_vim at yahoo dot com)
" Last Change: 12-Jan-2004 @ 19:18
" Created:     30-Nov-2001
" Requires: Vim-6.0
" Version: 1.5.2
" Acknowledgements:
"   Tom Regner {tomte at tomsdiner dot org}: Enhanced to work even when
"     foldmethod is not 'manual'.
"   John A. Peters {japeters at pacbell dot net} for giving feedback and
"     giving the idea of supporting a "-1" context.
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Download From:
"     http://www.vim.org/script.php?script_id=158
" Description:
"   Defines useful commands for the ease of creating folds. When the commands
"     are executed, if the current 'foldmethod' is not "manual", the current
"     value is saved in a buffer local variable and it is set to "manual" to
"     be able to create the folds. When you want to restore the original
"     'foldmethod', just execute the :FoldEndFolding command or set it to any
"     value that you wish. For help on working with folds, see help on
"     |folding|.
"   
"     FoldNonMatching - Pass an optional pattern and the number of context lines
"                       to be shown. Useful to see only the matching lines
"                       with or without context. You can give a range too.
"         Syntax:
"           [range]FoldNonMatching [pattern] [context]
" 
"         Ex:
"           50,500FoldNonMatching xyz 3
"   
"     FoldShowLines - Pass a comma separated list of line numbers and an
"                     optional number of context lines to be shown. All the
"                     rest of the lines (excluding those in context) will be
"                     folded away. You can give a range too.
"         Syntax:
"           [range]FoldShowLines {lines} [context]
"   
"         Ex:
"           50,500FoldShowLines 10,50,100 3
"   
"       The defaults for pattern and context are current search pattern
"         (extracted from / register) and 0 (for no context) respectively. A
"         value of -1 for context is treated specially because:
"	    - it creates folds with the matched lines as the starting line.
"	    - it sets 'foldminlines' to 0 to show all the folds as closed.
"	    - it sets 'foldtext' to display only the matched line for the fold
"	      (no "number of lines" or dashes as prefix). This allows you to
"	      view the matched lines more clearly and also follow the same
"	      indentation as the original (nice when folding code). You can
"	      however set g:foldutilFoldText to any value that is acceptable
"	      for 'foldtext' and customize this behavior (e.g., to view the
"	      number of lines in addition to the matched line, set it to
"	      something like:
"	      "getline(v:foldstart).' <'.(v:foldend-v:foldstart+1).' lines>'").
"
"	Ex: 
"	  - Open a vim script and try: 
"	      FoldNonMatching \<function\> -1
"	  - Open a java class and try: 
"	      FoldNonMatching public\|private\|protected -1
"
"	  Please send me other uses that you found for the "-1" context and I
"	  will add it to the above list.
"
"   You can change the default for context by setting g:foldutilDefContext.
"   The plugin by default, first clears the existing folds before creating the
"     new folds. But you can change this by setting g:foldutilClearFolds to 0,
"     in which case the new folds are added to the existing folds, so you can
"     create folds incrementally.
"
"   NOTE: The plugin doesn't use FUInitialize command to change the settings
"     any more, so when you need to change a setting, just change the
"     corresponding global variable.
"
" Summary Of Features:
"   Command Line:
"     FoldNonMatching (or FoldShowMatching), FoldShowLines, FoldEndFolding
"
"   Settings:
"       g:foldutilDefContext, g:foldutilClearFolds, g:foldutilFoldText
" TODO:

if exists("loaded_foldutil")
  finish
endif
let loaded_foldutil=1

" Initializations {{{

if ! exists("g:foldutilDefContext")
  let g:foldutilDefContext = 1
endif

if ! exists("g:foldutilClearFolds")
  " First eliminate all the existing folds, by default.
  let g:foldutilClearFolds = 1
endif

" 'foldtext' to be used for '-1' context case.
if ! exists("g:foldutilFoldText")
  let g:foldutilFoldText = 'getline(v:foldstart)'
endif

command! -range=% -nargs=* FoldNonMatching <line1>,<line2>:call <SID>FoldNonMatchingIF(<f-args>)
command! -range=% -nargs=* FoldShowMatching <line1>,<line2>FoldNonMatching <args>
command! -range=% -nargs=+ FoldShowLines <line1>,<line2>:call <SID>FoldShowLines(<f-args>)
command! FoldEndFolding :call <SID>EndFolding()

" Initializations }}}

" Interface method for the ease of defining a simpler command interface.
function! s:FoldNonMatchingIF(...) range
  call s:BeginFolding()
  if a:0 > 2
    echohl Error | echo "Too many arguments" | echohl None
    return
  endif

  if exists("a:1") && a:1 != ""
    let pattern = a:1
  elseif @/ == ""
    echohl Error | echo "No previous search pattern exists. Pass a search " .
	  \ "pattern as argument or do a search using the / command before " .
	  \ "re-running this command" | echohl None
    return
  else
    " If there is no pattern provided, use the current / register.
    let pattern = @/
  endif
  if exists("a:2") && a:2 != ""
    let context = a:2
  else
    let context = g:foldutilDefContext
  endif

  call s:FoldNonMatching(a:firstline, a:lastline, pattern, context)
endfunction

function! s:FoldShowLines(lines, ...) range
  call s:BeginFolding()
  if a:0 > 2
    echohl Error | echo "Too many arguments" | echohl None
    return
  endif

  if exists("a:1") && a:1 != ""
    let context = a:1
  else
    let context = g:foldutilDefContext
  endif

  if match(a:lines, ',$') == -1
    let pattern = a:lines . ','
  else
    let pattern = a:lines
  endif
  let pattern = substitute(pattern, '\(\d\+\),', '\\%\1l.*\\|', 'g')
  let pattern = substitute(pattern, '\\|$', '', '')

  call s:FoldNonMatching(a:firstline, a:lastline, pattern, context)
endfunction

"
" Fold all the non-matching lines. No error checks.
"
function! s:FoldNonMatching(fline, lline, pattern, context)
  let pattern = a:pattern

  " Since the way it is written, context = 1 actually means no context, but
  "    since this will look awkward to the user, it is adjusted here.
  "    Adding 1 below incidently takes care of the case when a non-numeric is
  "    passed
  let context = a:context
  let zeroContext = 0
  " If there is no context provided, use *no context*.
  if context < 0
    let context = 0
    let zeroContext = 1
    call s:SaveSetting('foldminlines')
    setl foldminlines=0
    call s:SaveSetting('foldtext')
    let &l:foldtext=g:foldutilFoldText
  else
    let context = context + 1
  endif

  if g:foldutilClearFolds
    " First eliminate all the existing folds.
    normal zE
  endif

  0
  let line1 = a:fline
  let foldCount = 0
  " So that when there are no matches, we can avoid creating one big fold.
  let matchFound = 0
  while line1 < a:lline
    " After advancing, make sure we are not still within the context of
    "   previous  match.
    let line2 = search(pattern, "bW")
    exec line1
    if line2 > 0 && line1 != line2 && (line1 - line2) < context
      let line1 = line2 + (context < 1 ? 1 : context)
      " Let us try again, as we may still be within the context.
      continue
    endif

    if match(getline('.'), pattern) != -1 && ! zeroContext
      let line2 = line('.')
    else
      +
      normal 0
      let line2 = search(pattern, "W")
    endif
    " No more hits.
    if line2 <= 0
      if matchFound
        " Adjust for the last line. Increment because we decrement below.
        let line2 = a:lline + (context < 1 ? 1 : context)
      else
        " No folds to create.
        break
      endif
    else
      let matchFound = 1
    endif
    if line2 != line1 && (line2 - line1) > context
      " Create a new fold.
      "call input("creating fold: line1 = " . line1 . " line2: " . (line2 - context) . ":")
      if zeroContext
	let line2 = line2 - 1
      endif
      exec line1.",".(line2 - context)."fold"
      let foldCount = foldCount + 1
    endif
    let line1 = line2 + (context < 1 ? 1 : context)
    exec line1
  endwhile
  redraw | echo "Folds created: " . foldCount
endfunction


" utility-functions

function! s:SaveSetting(setting)
  exec 'let curVal = &l:'.a:setting
  let w:settStr = 'let &l:'.a:setting."='".curVal."'\n" . w:settStr
endfunction

function! s:RestoreSettings()
  exec w:settStr
endfunction

function! s:BeginFolding()
  if !exists('w:settStr')
    let w:settStr = ''
    call s:SaveSetting('foldmethod')
    setl foldmethod=manual
  endif
endfunction

function! s:EndFolding()
  call s:RestoreSettings()
  unlet w:settStr
endfunction

" vim6:fdm=marker
