" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/MarkLines.vim	[[[1
295
" GetLatestVimScripts: 2028 1 :AUTOINSTALL: MarkLines

" MarkLines:  Allows you to toggle highlighting on any number of lines
"             in any number of windows.
" Author:     Matthew Wozniski (mjw@drexel.edu)
" Date:       September 25, 2007
" Version:    1.0
" History:    see :help marklines-history
" License:    BSD.  Completely open source, but I would like to be
"             credited if you use some of this code elsewhere.

" Copyright (c) 2007, Matthew J. Wozniski                                {{{1
" All rights reserved.
" 
" Redistribution and use in source and binary forms, with or without
" modification, are permitted provided that the following conditions are met:
"     * Redistributions of source code must retain the above copyright
"       notice, this list of conditions and the following disclaimer.
"     * Redistributions in binary form must reproduce the above copyright
"       notice, this list of conditions and the following disclaimer in the
"       documentation and/or other materials provided with the distribution.
"     * The names of the contributors may not be used to endorse or promote
"       products derived from this software without specific prior written
"       permission.
" 
" THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY
" EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
" WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
" DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
" DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
" (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
" LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
" ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
" (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
" SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

" Abort if running in vi-compatible mode or the user doesn't want us.    {{{1
if &cp || exists('g:marklines_loaded')
  if &cp && &verbose
    echo "Not loading MarkLines in compatible mode."
  endif
  finish
endif

let g:marklines_loaded = 1

" Choose an operating mode.                                              {{{1
" Normally, we will prefer to use matchadd(), then :2match, then :match, but
" the user may choose to override and choose himself.  Realistically, the user
" should never need to touch this.  matchadd() is undisputably better than the
" others, and I can see no reason why you would ever not want to use it if you
" had it.  However, it might be useful to force us to use :match instead of
" :2match if another plugin is already using :2match.
if ! exists('g:marklines_mode') || g:marklines_mode < 1 || g:marklines_mode > 3
  if exists('*matchadd')
    let g:marklines_mode=3
  elseif exists(':2match')
    let g:marklines_mode=2
  else
    let g:marklines_mode=1
  endif
  if &verbose
    echo "MarkLines will be operating in mode " . g:marklines_mode
  endif
endif

" Define a pair of AddMatch and ClearMatches functions for this mode.    {{{1
" AddMatch adds a new highlight for a window.
" ClearMatches clears all added highlights for a window.
if g:marklines_mode == 3
  " We're using matchadd, so we can create an arbitrary number of different
  " matches in each window, each containing an arbitrary number of lines and
  " each with its own highlight group.  As a result, we need to store a
  " window-local list of match numbers updated by AddMatch, and clear all of
  " those matches for ClearMatches.
  function! s:AddMatch(higrp, linestring)
    let id = matchadd(a:higrp, a:linestring)
    if(id == -1)   " Failed to add the new match.
      return 1     " Alert the caller
    endif
    call add(w:matchnrs, id)
  endfunction

  function! s:ClearMatches()
    if exists('w:matchnrs')
      for match in w:matchnrs
        call matchdelete(match)
      endfor
    endif
    let w:matchnrs = []
  endfunction
elseif g:marklines_mode == 2
  " Only one match at a time.  Just turn it off for ClearMatches
  function! s:AddMatch(higrp, linestring)
    exe '2match ' . a:higrp . ' /' . a:linestring . '/'
    return matcharg(2) == ['','']   " Failed to add the match
  endfunction

  function! s:ClearMatches()
    2match none
  endfunction
else
  " Only one match at a time.  Just turn it off for ClearMatches
  function! s:AddMatch(higrp, linestring)
    exe 'match '  . a:higrp . ' /' . a:linestring . '/'
    return matcharg(1) == ['','']   " Failed to add the match
  endfunction

  function! s:ClearMatches()
    match none
  endfunction
endif

" Define 's:RefreshWindow' for updating the current window's highlight.  {{{1
" Switch to lazy window repainting, clear the existing highlighting, add the
" new highlighting, and change the window repainting method back.
function! s:RefreshWindow()
  let savelz = (&lz ? 'lz' : 'nolz')
  set lz
  call s:ClearMatches()
  call s:UpdateMarkedLines()
  exe 'set ' . savelz
endfunction

" Define 's:RefreshAllWindows' for refreshing every window on a tab.     {{{1
" Restore the current window afterwards.
function! s:RefreshAllWindows()
  let currwin = winnr()
  windo call s:RefreshWindow()
  exe currwin . "wincmd w"
endfunction


" Define 's:UpdateMarkedLines' to call AddMatch once per highlight.      {{{1
" If there are lines to be marked, then mark each of them.  Remove the
" highlight group from the marked lines if adding the match failed, to prevent
" the same error from occurring every time we redraw.
function! s:UpdateMarkedLines()
  if exists('b:match_strings') && len(b:match_strings) != 0
    for key in keys(b:match_strings)
      if strlen(b:match_strings[key])
        let rv = s:AddMatch(key, b:match_strings[key])
        if rv == 1
          let b:marked_lines[key] = []
          let b:match_strings[key] = ""
        endif
      endif
    endfor
  endif
endfunction

" Define 's:SetMarked' to mark or unmark the current line or range.      {{{1
" Chooses a highlight group: either the arg passed in, or the value of
" g:marklines_highlight, or 'MarkedLine', in that order.  If the mode argument
" is zero, unmarks all lines in the range, with the side effect of modifying
" the current highlight color if VIM doesn't support matchadd.  If the mode
" argument is 1, marks all lines in the range.  Otherwise, toggles: If all
" chosen lines are already marked with the chosen highlight group, then unmark
" them all.  Otherwise, mark them all with that highlight group.  Then,
" refresh all windows on this tab, in case the current buffer is shown in
" multiple windows.
function! s:SetMarked(mode, ...) range
  " Choose a highlight color
  if a:0 == 0 && exists('g:marklines_highlight')
    let key = g:marklines_highlight
  elseif a:0 == 0
    let key = 'MarkedLine'
  else
    let key = join(a:000, ' ')
  endif

  " Initialize dictionary
  if ! exists('b:marked_lines')
    let b:marked_lines = {}
  endif

  " Initialize array at this key
  if ! exists('b:marked_lines[key]')
    let b:marked_lines[key] = []
  endif

  " Decide if we're turning these lines on or off.
  if a:mode == 1
    let turningoff = 0
  elseif a:mode == 0
    let turningoff = 1
  else
    " If they're all on with this highlight group, off, else on.
    let turningoff=1
    for i in range(a:firstline, a:lastline)
      if index(b:marked_lines[key], i) == -1
        let turningoff=0
        break
      endif
    endfor
  endif

  " Remove these lines from other highlight groups.
  " Then add them to this highlight group unless we're toggling this area off.
  for i in range(a:firstline, a:lastline)
    for k in keys(b:marked_lines)
      if a:mode != 0 || (a:0 == 0 || k == key)
        call filter(b:marked_lines[k], 'v:val != i')
      endif
      if k != key && g:marklines_mode != 3 && !turningoff
        " This is the wrong key, and we can only have one key. Move the vals.
        let b:marked_lines[key] += b:marked_lines[k]
        unlet b:marked_lines[k]
      endif
    endfor

    if !turningoff && i > 1 && i < line("$")
      call add(b:marked_lines[key], i)
    endif
  endfor

  " A bit of cleanup: remove empty keys from the dictionary
  call filter(b:marked_lines, 'v:val != []')

  " Create strings suitable as an argument to matchadd from the match numbers
  let b:match_strings = s:GenerateMatchStrings(b:marked_lines)

  " Refresh all windows on this tab page
  call s:RefreshAllWindows()
endfunction

" Define 's:GenerateMatchStrings' to get a matchadd arg from line nums.  {{{1
" The returned dictionary has the same keys as the arg passed in, and the
" values are simply converted from arrays of integers to strings of multi-line
" matches suitable for matchadd().
function s:GenerateMatchStrings(dict)
  let rv = {}
  if len(a:dict)
    for key in keys(a:dict)
      if len(a:dict[key])
        let rv[key] = '\%' . join(a:dict[key], 'l\|\%') . 'l'
      endif
    endfor
  endif
  return rv
endfunction

" Define autocommands to refresh windows based on user interaction.      {{{1
" Refresh all windows when switching tab pages.
" Refresh the current window when changing the buffer inside it.
if ! exists('s:autocmds_loaded')
  autocmd BufWinEnter * call <SID>RefreshWindow()
  autocmd TabEnter    * call <SID>RefreshAllWindows()
  let s:autocmds_loaded = 1
endif

" Define ':MarkLinesOn' for unconditionally marking a range.             {{{1
" :MarkLinesOn takes 0 or 1 arg; the optional arg to SetMarked.
" It takes a range, and defaults to the current line if one is not provided.
" It can be followed by another command, and tab-completes highlight groups.
command -nargs=? -range -bar -complete=highlight MarkLinesOn :<line1>,<line2>call <SID>SetMarked(1, <f-args>)

" Define ':MarkLinesOff' for unconditionally unmarking a range.          {{{1
" :MarkLinesOff takes 0 or 1 arg; the optional arg to SetMarked.
" It takes a range, and defaults to the current line if one is not provided.
" It can be followed by another command, and tab-completes highlight groups.
command -nargs=? -range -bar -complete=highlight MarkLinesOff :<line1>,<line2>call <SID>SetMarked(0, <f-args>)

" Define ':MarkLinesToggle' for conditionally marking a range.           {{{1
" :MarkLinesToggle takes 0 or 1 arg; the optional highlight group to SetMarked.
" It takes a range, and defaults to the current line if one is not provided.
" It can be followed by another command, and tab-completes highlight groups.
" If all lines in the range are already marked with the chosen highlight
" group, turn unmark them all, else mark them all.
command -nargs=? -range -bar -complete=highlight MarkLinesToggle :<line1>,<line2>call <SID>SetMarked(2, <f-args>)

" Define convenience maps.                                               {{{1
if ! exists('g:marklines_noautomap')
  nmap <silent> <unique> <leader>mc :MarkLinesOff<CR>
  nmap <silent> <unique> <leader>ms :MarkLinesOn<CR>
  nmap <silent> <unique> <leader>mt :MarkLinesToggle<CR>
  nmap <silent> <unique> <leader>me :MarkLinesToggle ErrorMsg<CR>
  vmap <silent> <unique> <leader>mc :MarkLinesOff<CR>
  vmap <silent> <unique> <leader>ms :MarkLinesOn<CR>
  vmap <silent> <unique> <leader>mt :MarkLinesToggle<CR>
  vmap <silent> <unique> <leader>me :MarkLinesToggle ErrorMsg<CR>
endif

" Define the MarkedLine highlight group unless its been defined.         {{{1
if hlID('MarkedLine') == 0
  if &bg == 'dark' && &t_Co == 256
    highlight MarkedLine ctermbg=237 guibg=#3a3a3a
  elseif &bg == 'dark'
    highlight MarkedLine ctermbg=8 guibg=#3a3a3a
  elseif &t_Co == 256
    highlight MarkedLine ctermbg=248 guibg=#a8a8a8
  else
    highlight MarkedLine ctermbg=7 guibg=#a8a8a8
  endif
endif
doc/MarkLines.txt	[[[1
219
*MarkLines.txt*         Visually mark important lines   Sep 22, 2007

                                                           *marklines-author*
Author:  Matthew J. Wozniski (mjw@drexel.edu)
                                                          *marklines-license*
Copyright (c) 2007, Matthew J. Wozniski
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The names of the contributors may not be used to endorse or promote
      products derived from this software without specific prior written
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=============================================================================
1. Contents                                  *marklines* *marklines-contents*

  1. Contents                       |marklines-contents|
  2. Introduction                   |marklines-intro|
  3. Usage                          |marklines-usage|
    3.1. Common usage               |marklines-common|
    3.2. MarkLinesOn usage          |:MarkLinesOn|
    3.3. MarkLinesOff usage         |:MarkLinesOff|
    3.4. MarkLinesToggle usage      |:MarkLinesToggle|
    3.5. Usage examples             |marklines-examples|
  4. Mapped keys                    |marklines-maps|
  5. Customization                  |marklines-customization|
    5.1. Variables                  |marklines-variables|
    5.2. Default Highlight Group    |marklines-default-higrp|
  6. To do                          |marklines-todo|
  7. History                        |marklines-history|

=============================================================================
2. Introduction                                             *marklines-intro*

The MarkLines plugin allows one to visually mark any number of lines, in any
number of buffers, with any chosen highlight.  In a sufficiently new version
of VIM, 7.1.040 or greater, you can use more than one highlight color per
window simultaneously.

=============================================================================
3. Usage                                                    *marklines-usage*

Let's keep this simple and sweet.  This plugin provides three new commands,
|:MarkLinesOn|, |:MarkLinesOff| and |:MarkLinesToggle|.

3.1. Common usage                                          *marklines-common*

All three new commands operate upon a range (see |10.3| or |cmdline-ranges|).
If a range is not provided, all three default to the current line.  An invalid
range simply stops any lines from being marked or unmarked.  All three also
take an optional argument representing a highlight group.  If it is not given
and the global variable |g:marklines_highlight| is defined, it defaults to the
value of that variable, otherwise it defaults to the highlight group
|MarkedLine|.  When marking a range, the chosen highlight group is used.  For
:MarkLinesOff, a line is only unmarked if it matches the provided highlight
group.  :MarkLinesOff with no arguments unmarks all lines.

3.2. :MarkLinesOn                                              *:MarkLinesOn*

:MarkLinesOn will take every line in the range provided to it and mark it with
the provided highlight group, chosen as explained in |marklines-common|.  If
your version of VIM only supports one active match per plugin per window and
another highlight group was already being used, all lines already marked with
the other highlight group will now be marked with the new highlight group.
This color squeezing will occur even if the given range was invalid.

3.3. :MarkLinesOff                                            *:MarkLinesOff*

:MarkLinesOff will take every line in the range provided to it and unmark it
if it matches the highlight group passed as an argument to :MarkLinesOff.  If
:MarkLinesOff was not passed an argument, it unmarks all lines in the range
regardless of what highlight group each was previously in.  If a line was not
in a highlight group, it remains unchanged.

3.4. :MarkLinesToggle                                      *:MarkLinesToggle*

:MarkLinesToggle is the hybrid between :MarkLinesOn and :MarkLinesOff.  If
every line in the range provided to it is already in the chosen highlight
group, determined as per |marklines-common|, it is equivalent to calling
:MarkLinesOff for the given range.  Otherwise, it is equivalent to calling
:MarkLinesOn for the given range with the given highlight group.

3.5. Examples                                            *marklines-examples*

The following are examples of useful ways to call these functions.

  " Mark the current line with the default highlight background
:MarkLinesOn

  " Mark the current line with red foreground text
:MarkLinesOn Error

  " Mark all lines but the first with the custom highlight group MyGroup
:2,$MarkLinesOn MyGroup

  " If the current line is already marked with an Error highlight, unmark it.
  " Otherwise, mark the current line with an Error highlight.
:MarkLinesToggle Error

  " Unmark all marked lines
:%MarkLinesOff

  " Unmark all lines currently marked with an error highlight.
:%MarkLinesOff Error

  " Mark the entirety of every line containing the word 'kibab' after
  " unmarking all other lines
:MarkLinesClear | g/kibab/:MarkLines

  " And, what help page would be complete without a quick-and-dirty hack?  If
  " your VIM version supports it, you can have multiple different highlight
  " groups for matched lines within a single window at the same time.  If you
  " want to combine all of those into one highlight group, you can force it
  " like this:
:let g:marklines_mode=2 | :0MarkLinesOn Error | let g:marklines_mode=3

The above works by temporarily tricking MarkLines into thinking that it only
supports one match at a time, convincing it to squeeze highlight groups but
not add any new lines by calling MarkLinesOn with an invalid range.  Then, it
restores the old operating mode.

=============================================================================

4. Mapped keys                                               *marklines-maps*

If |g:marklines_noautomap| is not set when MarkLines is loaded, MarkLines will
create a few default maps to make using MarkLines easier.  These maps are
bound to |mapleader| followed by two keystrokes.  If the 'mapleader' variable
is unset, they begin with a backslash, "\".  They are mapped for both normal
and visual mode, so it's easy to operate upon a selection.

<leader>mc   Clear:  Call MarkLinesOff for the current line or selection.
<leader>ms   Select: Call MarkLinesOn for the current line or selection.
<leader>mt   Toggle: Call MarkLinesToggle for the current line or selection.
<leader>me   Error:  Like \mt but using the "ErrorMsg" group, rather than the
                     default highlight group.

=============================================================================

5. Customization                                    *marklines-customization*

5.1. Variables                                          *marklines-variables*

Marklines can be customized a bit by defining certain global variables in your
.vimrc.

let g:marklines_highlight = "ErrorMsg"                *g:marklines_highlight*

This variable controls the default highlight group.  If :MarkLinesOn or
:MarkLinesToggle is called with no highlight group argument and this variable
is defined, the contents of this variable is used as the highlight group name.

let g:marklines_noautomap = 1                         *g:marklines_noautomap*

This variable being set prevents MarkLines from creating its maps.  Useful if
you are already binding your own to different keys and don't want duplication.

let g:marklines_loaded = 1                               *g:marklines_loaded*

This variable would prevent MarkLines from being loaded.  But why would you
want to do that?  Only valid if present before MarkLines is loaded.

let g:marklines_mode = 3 (or 2 or 1)                       *g:marklines_mode*

This variable controls which match functions are used by MarkLines.  Normally
you shouldn't have to touch this.  MarkLines will detect on its own if you
have matchadd(), and will use it if you do.  Otherwise, if you have :2match,
it will use :2match.  Otherwise, it will use :match.  Setting this variable to
3 forces the use of matchadd()  That almost certainly won't work if it wasn't
detected automatically.  Setting this variable to 2 forces the use of :2match.
That is probably not what you want if you have matchadd(), since matchadd()
gives extra features like the ability to have multiple matches per plugin per
window.  Setting this variable to 1 forces the use of :match.  The only
legitimate use I can see for this variable is for testing for backwards
compatibility, or for forcing MarkLines to use :match instead of :2match if
another plugin is using :2match.  Only valid if present before MarkLines is
loaded, though can be changed from 3 to something else later to force color
squeezing as per :MarkLinesOn.

5.2. Default Highlight Group           *MarkedLine* *marklines-default-higrp*

If |g:marklines_highlight| is unset, you can set the default highlight group
using, for example, 

:hi MarkedLine ctermfg=12 guifg=#ff8030 ctermbg=3 guibg=#ff0000

See |:hi| for more details.

=============================================================================

6. To do                                                     *marklines-todo*

Not really a major thing, but it would be nice to be able to do characterwise,
rather than linewise, marking.  Perhaps if people like this plugin and would
like it extended.

=============================================================================

7. History                                                *marklines-history*

1.0   Sep 25, 07   Kibab asked for a plugin that did this in #vim, and a week
                   and a lot of documentation later, here we are.
