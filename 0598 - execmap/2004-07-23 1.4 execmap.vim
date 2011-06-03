" execmap.vim - Lets you relax while executing long normal mode maps 
" Author: Hari Krishna Dara (hari_vim at yahoo dot com)
" Last Change: 31-Mar-2004 @ 23:58
" Created:     17-Mar-2003
" Requires: Vim-6.3, genutils.vim (1.10)
" Version: 1.4.1
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
"   The plugin provided a generic functions called ExecMap() and ExecMap2()
"   which need to be defined as the handler for the set of maps starting with
"   a common prefix. It by default defines this as the handler for the prefix
"   "\" and "_" (without the quotes) so that for all the mappings that start
"   with these prefixes, the plugin provides a backup on errors and time out
"   cases. But you can define additional mappings for additional prefixes such
"   as "," etc.
"
"	nnoremap , :call ExecMap(',')<CR>
"
"   As you enter key strokes to complete a command, the plugin looks up the
"   partial command in the complete list of normal or visual mode mapping (as
"   the per the case) and rejects the key strokes if it doesn't match with
"   any. When the command matches an existing map with no conflicts, it gets
"   immediately executed. When the command matches a map, but there is a
"   longer map that could be matched, the plugin indicates this by displaying
"   a "*" at which point you can press <Enter> to execute the map, or continue
"   to type to match the longer command. Here is an example to describe it
"   better, say you have two maps as below:
"
"	nnoremap \ma :echo "ma"<CR>
"	nnoremap \maxi :echo "maxi"<CR>
"
"   Typing "\ma" would match one of the mappings completely, but it would also
"   match "\maxi" partially, so the plugin allows you to continue to type
"   until you press <CR>. However, whenever there is an exact match for the
"   current command with an existing map, the plugin indicates it by appending
"   a "*" to the prompt (pressing <CR> at that moment would execute a valid
"   command). I have found it working well for both normal and visual mode
"   mappings.
"
"   You can use <BS> to clear the previous character and press <Esc> or <C-C>
"   to cancel it anytime. You can also press <Enter> to execute the command
"   that is typed in, but it has to match a valid map.
"
"   Note that this functionality was originally part of genutils plugin, but
"   it is more appropriate for it to be a separate plugin rather than a
"   library function so it is no longer part of genutils.
"
" Function Prototypes:
"     " Add a handler for all the maps that share the given prefix in
"     "	  normal mode.
"     void ExecMap(String prefix)
"
"     " Add a handler for all the maps that share the given prefix in
"     "	  the given mode. Only normal and visual modes are supported.
"     void ExecMap2(String prefix, String mode)
"
"     " Prompt user for a map with the given prefix in the given mode and
"     "	  return the map. The return value could be executed using :normal
"     "	  command. If the user cancels the operation, it returns an empty
"     "	  string. Only normal and visual modes are supported.
"     String ExecMapPrompt(String prefix, String mode)
" Limitations:
"   - You can only execute maps that are complete to be executable using
"     :normal exmode command (see help on :normal). One example map that will
"     not work is:
"
"	nnoremap _ab :call input('ab command')<CR>
"
"     If you execute the above command using ":normal _ab", it will not run as
"     expected. Similarly, maps that are supposed to leave you on the : prompt
"     don't work as well.
" TODO:

if exists("loaded_execmap")
  finish
endif
if v:version < 603
  echomsg 'execmap: You need at least Vim 6.3'
  finish
endif

if !exists("loaded_genutils")
  runtime plugin/genutils.vim
endif
if !exists("loaded_genutils") || loaded_genutils < 110
  echomsg "execmap: You need a newer version of genutils.vim plugin"
  finish
endif
let loaded_execmap=104

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

function! ExecMap2(prefix, mode) range
  let mapCmd = ExecMapPrompt(a:prefix, a:mode)
  if mapCmd != ''
    exec "normal ".mapCmd
  endif
endfunction

" Receives a spurious key stroke in visual mode.
function! ExecMapPrompt(prefix, mode) range
  " Temporarily remove the mapping, otherwise it will interfere with the
  " mapcheck call below:
  if a:prefix != ''
    let myMap = maparg(a:prefix, a:mode)
    if myMap != ''
      exec a:mode . "unmap" a:prefix
      "echoerr "In ExecMap2: prefix: " . a:prefix . ' mode: ' . a:mode . ' myMap: ' . maparg(a:prefix, a:mode)
    endif
  endif

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
    try
      let char = getchar()
    catch /^Vim:Interrupt$/
      let char = "\<Esc>"
    endtry
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
    let mapCmd = gotoc.mapCmd
  else
    let mapCmd = ''
  endif
  if a:prefix != '' && myMap != ''
    exec a:mode . "noremap" a:prefix myMap
  endif
  "echomsg "Leaving ExecMap2: prefix: " . a:prefix . ' mode: ' . a:mode . ' myMap: ' . maparg(a:prefix, a:mode)
  return mapCmd
endfunction

function! s:Prompt(spe, mapCmd)
  echon "\rEnter Map" . a:spe . ": " . a:mapCmd
endfunction
