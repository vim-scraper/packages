" execmap.vim - Lets you relax while executing long normal mode maps 
" Author: Hari Krishna Dara <hari_vim at yahoo dot com>
" Last Change: 22-Mar-2003 @ 13:59PM
" Created:     17-Mar-2003
" Requires: Vim-6.0
" Version: 1.0.2
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Download From:
"     http://www.vim.org/script.php?script_id=598
" Description:
"   The aim of this plugin is to allow users to define lengthy normal mode
"   mappings (and thus increase the name-space) and still be able to type the
"   mappings comfortably, thus reducing the chance of making mistakes or
"   getting timed out. When you make mistakes in typing (resulting in an
"   invalid map) or pause for long enough, the control goes to the plugin
"   instead of getting aborted, which then prompts you to continue typing or
"   correct the map, without needing to start all over.
"
"   The plugin provided a generic function called ExecMap() which needs to be
"   defined as the handler for the set of maps starting with a common prefix.
"   It by default defines this as the handler for the prefix "\" and "_"
"   (without the quotes) so that for all the mappings that start with these
"   prefixes, the plugin provides a backup on errors and time out cases. But
"   you can define additional mappings for additional prefixes such as ","
"   etc.
"
"	nnoremap , :call ExecMap(',')<CR>
"
"   This works only for normal mode mappings as of now. This function was
"   originally part of genutils plugin, but it is more appropriate for it to
"   be a plugin rather than a library function.
"
" TODO:
"   - ExecMap doesn't work property on vmaps.

if exists("loaded_execmap")
  finish
endif
let loaded_execmap=1


if (! exists("no_plugin_maps") || ! no_plugin_maps) &&
      \ (! exists("no_execmap_maps") || ! no_execmap_maps)
   nnoremap \ :call ExecMap('\')<CR>
   nnoremap _ :call ExecMap('_')<CR>
endif


" Reads a normal mode mapping at the command line and executes it with the
"   given prefix. Press <BS> to correct and <Esc> to cancel.
function! ExecMap(prefix)
  call ExecMap2(a:prefix, 'n')
endfunction

" Receives a spurious key stroke in visual mode.
function! ExecMap2(prefix, mode)
  " Temporarily remove the mapping, otherwise it will interfere with the
  " mapcheck call below:
  let myMap = maparg(a:prefix, a:mode)
  exec a:mode . "unmap" a:prefix
  "echoerr "In ExecMap2: prefix: " . a:prefix . ' mode: ' . a:mode . ' myMap: ' . maparg(a:prefix, a:mode)

  " Generate a line with spaces to clear the previous message.
  let i = 1
  let clearLine = "\r"
  while i < &columns
    let clearLine = clearLine . ' '
    let i = i + 1
  endwhile

  let mapCmd = a:prefix
  let foundMap = 0
  let breakLoop = 0
  let curMatch = ''
  echon "\rEnter Map: " . mapCmd
  while !breakLoop
    let char = getchar()
    if char !~ '^\d\+$'
      if char == "\<BS>"
	let mapCmd = strpart(mapCmd, 0, strlen(mapCmd) - 1)
      endif
    else " It is the ascii code.
      let char = nr2char(char)
      if char == "\<Esc>"
	let breakLoop = 1
      "elseif char == "\<CR>"
	"let mapCmd = curMatch
	"let foundMap = 1
	"let breakLoop = 1
      else
	let mapCmd = mapCmd . char
	if maparg(mapCmd, a:mode) != ""
	  let foundMap = 1
	  let breakLoop = 1
	else
	  let curMatch = mapcheck(mapCmd, a:mode)
	  if curMatch == ""
	    let mapCmd = strpart(mapCmd, 0, strlen(mapCmd) - 1)
	  endif
	endif
      endif
    endif
    echon clearLine
    "echon "\rEnter Map: " . substitute(mapCmd, '.', ' ', 'g') . "\t" . curMatch
    echon "\rEnter Map: " . mapCmd
  endwhile
  if foundMap
    if a:mode == 'v'
      normal gv
    endif
    exec "normal" mapCmd
  endif
  exec a:mode . "noremap" a:prefix myMap
  "echomsg "Leaving ExecMap2: prefix: " . a:prefix . ' mode: ' . a:mode . ' myMap: ' . maparg(a:prefix, a:mode)
endfunction
