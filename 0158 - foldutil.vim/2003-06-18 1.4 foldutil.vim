" foldutil.vim -- utilities for creating folds.
" Author: Hari Krishna <hari_vim at yahoo dot com>
" Last Change: 12-Jun-2003 @ 13:19
" Created:     30-Nov-2001
" Requires: Vim-6.0
" Version: 1.4.0
" Acknowledgements:
"   Tom Regner {tomteat tomsdiner dot org}: Enhanced to work even when
"     foldmethod is not 'manual'.
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
"         (extracted from / register) and 0 (for no context) respectively.
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
"     FoldNonMatching, FoldShowLines, FoldEndFolding
"
"   Settings:
"       g:foldutilDefContext, g:foldutilClearFolds
" TODO:
"

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

command! -range=% -nargs=* FoldNonMatching <line1>,<line2>:call <SID>FoldNonMatchingIF(<f-args>)
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
  let context = a:context + 1
  " If there is no context provided, use *no context*.
  if a:context < 0
    let context = 1
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
      let line1 = line2 + context
      " Let us try again, as we may still be within the context.
      continue
    endif

    if match(getline('.'), pattern) != -1
      let line2 = line('.')
    else
      let line2 = search(pattern, "W")
    endif
    " No more hits.
    if line2 <= 0
      if matchFound
        " Adjust for the last line. Increment because we decrement below.
        let line2 = a:lline + context
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
      exec line1 "," (line2 - context) "fold"
      let foldCount = foldCount + 1
    endif
    let line1 = line2 + context
    exec line1
  endwhile
  redraw | echo "Folds created: " . foldCount
endfunction


" utility-functions

function! s:BeginFolding()
  if !exists("b:fdmOrig")
    let b:fdmOrig = &foldmethod
    setl foldmethod=manual
  endif
endfunction

function! s:EndFolding()
  if exists("b:fdmOrig")
    let &foldmethod = b:fdmOrig
    unlet b:fdmOrig
  endif
endfunction

" vim6:fdm=marker
