" execmap.vim - Lets you relax while executing long normal mode maps 
" Author: Hari Krishna Dara (hari_vim at yahoo dot com)
" Last Change: 29-Mar-2004 @ 12:54
" Created:     17-Mar-2003
" Requires: Vim-6.2.293, genutils.vim (1.10)
" Version: 1.3.3
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Acknowledgements:
"     -	Srinath Avadhanula ( srinath at fastmail dot fm ) for improving the
"	script to work with visual mode mappings also (as part of his
"	vim-latex project).
"     -	Salmon Halim (salmanhalim at yahoo dot com) for reporting the problem
"	with ambiguous mappings and some ideas on resolving it.
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
"   At anytime as soon as the command that is typed matches an existing map
"   completely and if there are no other maps that would match partially, the
"   command is executed just like Vim would do. But if there are other maps
"   that would match this command partially, then you need to press <CR> to
"   let the plugin accept the command and execute it. Here is an example to
"   describe it better, say you have two maps as below:
"
"	nnoremap \ma :echo "ma"<CR>
"	nnoremap \maxi :echo "maxi"<CR>
"
"   Typing "\ma" would match one of the mappings completely, but it would also
"   match "\maxi" partially, so the plugin allows you to continue to type
"   until you press <CR>. However, whenever there is an exact match for the
"   current command with an existing map, the plugin indicates it by appending
"   a "*" to the prompt (pressing <CR> at that moment would execute a valid
"   command).
"
"   I have found it working well for both normal and visual mode mappings.
"   This function was originally part of genutils plugin, but it is more
"   appropriate for it to be a plugin rather than a library function.
"
" Limitations:
"   - You can only execute maps that are complete to be executable using
"     :normal exmode command (see help on :normal). One example map that will
"     not work is:
"
"	nnoremap _ab :call input('ab command')<CR>
"
"     If you execute the above command using ":normal _ab", it will not run as
"     expected.
" TODO:

if exists("loaded_execmap")
  finish
endif
let loaded_execmap=103

if !exists("loaded_genutils")
  runtime plugin/genutils.vim
endif
if !exists("loaded_genutils") || loaded_genutils < 110
  echomsg "execmap: You need a newer version of genutils.vim plugin"
  finish
endif

nnoremap <silent> <script> <Plug>ExecMapSelectRegion gv

if (! exists("no_plugin_maps") || ! no_plugin_maps) &&
      \ (! exists("no_execmap_maps") || ! no_execmap_maps)
   nnoremap \ :call ExecMap2('\', 'n')<CR>
   nnoremap _ :call ExecMap2('_', 'n')<CR>
   vnoremap \ :call ExecMap2('\', 'v')<CR>
   vnoremap _ :call ExecMap2('_', 'v')<CR>
endif


" Reads a normal mode mapping at the command line and executes it with the
"   given prefix. Press <BS> to correct and <Esc> to cancel.
function! ExecMap(prefix) range
  call ExecMap2(a:prefix, 'n')
endfunction

" Receives a spurious key stroke in visual mode.
function! ExecMap2(prefix, mode) range
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
  call s:Prompt('', mapCmd)
  while !breakLoop
    let char = getchar()
    "exec BPBreakIf(cnt == 1, 2)
    if char == '^\d\+$' || type(char) == 0
      let char = nr2char(char)
    endif " It is the ascii code.
    if char == "\<BS>"
      let mapCmd = strpart(mapCmd, 0, strlen(mapCmd) - 1)
      let foundMap = (maparg(mapCmd, a:mode) != "") ? 1 : 0
    elseif char == "\<Esc>"
      let breakLoop = 1
    elseif char == "\<CR>"
      "let mapCmd = curMatch
      let foundMap = 1
      let breakLoop = 1
    else
      let mapCmd = mapCmd . char
      if maparg(mapCmd, a:mode) != ""
	let foundMap = 1
	" Check if there is a longer match possible than the current mapCmd.
	let nMaxMaps = strlen(substitute(GetVimCmdOutput(a:mode.'map '.mapCmd),
	      \ "[^\n]", '', 'g')) - 1
	let nMaps = strlen(substitute(GetVimCmdOutput(a:mode.'map '.mapCmd.
	      \ 'ZZZZZZZZZZZZ'), "[^\n]", '', 'g')) - 1
	if nMaps == nMaxMaps
	  let breakLoop = 1
	endif
      else
	let foundMap = 0
	let curMatch = mapcheck(mapCmd, a:mode)
	if curMatch == ""
	  let mapCmd = strpart(mapCmd, 0, strlen(mapCmd) - 1)
	endif
      endif
    endif
    echon clearLine
    "echon "\rEnter Map: " . substitute(mapCmd, '.', ' ', 'g') . "\t" . curMatch
    call s:Prompt(foundMap ? '*' : '', mapCmd)
  endwhile
  if foundMap && mapCmd != ''
    let gotoc = ''
    if a:mode == 'v'
      " use a plug to select the region instead of using something like
      " gv or `<v`> to avoid problems caused by some of the characters
      " being mapped (we use :normal not :normal!).
      let gotoc = "\<Plug>ExecMapSelectRegion"
    endif
    exec "normal ".gotoc.mapCmd
  endif
  exec a:mode . "noremap" a:prefix myMap
  "echomsg "Leaving ExecMap2: prefix: " . a:prefix . ' mode: ' . a:mode . ' myMap: ' . maparg(a:prefix, a:mode)
endfunction

function! s:Prompt(spe, mapCmd)
  echon "\rEnter Map" . a:spe . ": " . a:mapCmd
endfunction
