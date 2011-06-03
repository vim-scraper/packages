" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/NrrwRgn.vim	[[[1
44
" NrrwRgn.vim - Narrow Region plugin for Vim
" -------------------------------------------------------------
" Version:	   0.11
" Maintainer:  Christian Brabandt <cb@256bit.org>
" Last Change: Wed, 28 Jul 2010 20:33:05 +0200
"
" Script: http://www.vim.org/scripts/script.php?script_id=3075 
" Copyright:   (c) 2009, 2010 by Christian Brabandt
"			   The VIM LICENSE applies to histwin.vim 
"			   (see |copyright|) except use "NrrwRgn.vim" 
"			   instead of "Vim".
"			   No warranty, express or implied.
"	 *** ***   Use At-Your-Own-Risk!   *** ***
" GetLatestVimScripts: 3075 11 :AutoInstall: NrrwRgn.vim
"
" Init: {{{1
let s:cpo= &cpo
if exists("g:loaded_nrrw_rgn") || &cp
  finish
endif
set cpo&vim
let g:loaded_nrrw_rgn = 1

" ------------------------------------------------------------------------------
" Public Interface: {{{1

" Define the Command:
com! -range NarrowRegion :exe ":" . <line1> . ',' . <line2> . "call nrrwrgn#NrrwRgn()"
com! -range NR	 :exe ":" . <line1> . ',' . <line2> . "call nrrwrgn#NrrwRgn()"
com! -range NRV  :call nrrwrgn#VisualNrrwRgn(visualmode())
com! NW	 :exe ":" . line('w0') . ',' . line('w$') . "call nrrwrgn#NrrwRgn()"
com! NarrowWindow :exe ":" . line('w0') . ',' . line('w$') . "call nrrwrgn#NrrwRgn()"

" Define the Mapping:
if !hasmapto('<Plug>NrrwrgnDo')
	xmap <unique> <Leader>nr <Plug>NrrwrgnDo
endif
xnoremap <unique> <script> <Plug>NrrwrgnDo <sid>VisualNrrwRgn
xnoremap <sid>VisualNrrwRgn :<c-u>call nrrwrgn#VisualNrrwRgn(visualmode())<cr>

" Restore: "{{{1
let &cpo=s:cpo
unlet s:cpo
" vim: ts=4 sts=4 fdm=marker com+=l\:\"
autoload/nrrwrgn.vim	[[[1
341
" NrrwRgn.vim - Narrow Region plugin for Vim
" -------------------------------------------------------------
" Version:	   0.11
" Maintainer:  Christian Brabandt <cb@256bit.org>
" Last Change: Wed, 28 Jul 2010 20:33:05 +0200
"
" Script: http://www.vim.org/scripts/script.php?script_id=3075 
" Copyright:   (c) 2009, 2010 by Christian Brabandt
"			   The VIM LICENSE applies to NrrwRgn.vim 
"			   (see |copyright|) except use "NrrwRgn.vim" 
"			   instead of "Vim".
"			   No warranty, express or implied.
"	 *** ***   Use At-Your-Own-Risk!   *** ***
" GetLatestVimScripts: 3075 11 :AutoInstall: NrrwRgn.vim
"
" Functions:

fun! <sid>WarningMsg(msg)"{{{1
	echohl WarningMsg
	let msg = "NarrowRegion: " . a:msg
	if exists(":unsilent") == 2
		unsilent echomsg msg
	else
		echomsg msg
	endif
	echohl Normal
	let v:errmsg = msg
endfun

fun! <sid>Init()"{{{1
    if !exists("s:instn")
		let s:instn=1
    else
		let s:instn+=1
    endif
	if !exists("s:nrrw_rgn_lines")
		let s:nrrw_rgn_lines = {}
	endif
	let s:nrrw_rgn_lines[s:instn] = {}
    let s:nrrw_winname='Narrow_Region'

    " Customization
    let s:nrrw_rgn_vert = (exists("g:nrrw_rgn_vert")  ? g:nrrw_rgn_vert   : 0)
    let s:nrrw_rgn_wdth = (exists("g:nrrw_rgn_wdth")  ? g:nrrw_rgn_wdth   : 20)
    let s:nrrw_rgn_hl   = (exists("g:nrrw_rgn_hl")    ? g:nrrw_rgn_hl     : "WildMenu")
    let s:nrrw_rgn_nohl = (exists("g:nrrw_rgn_nohl")  ? g:nrrw_rgn_nohl   : 0)

    let s:debug=1
	if exists("s:debug") && s:debug
		com! NI :call <sid>WarningMsg("Instance: ".s:instn)
		com! NJ :call <sid>WarningMsg("Data: ".string(s:nrrw_rgn_lines))
	endif
		
endfun 

fun! <sid>NrwRgnWin() "{{{1
	let s:nrrw_winname .= '_' . s:instn
    let nrrw_win = bufwinnr('^'.s:nrrw_winname.'$')
    if nrrw_win != -1
		exe ":noa " . nrrw_win . 'wincmd w'
		silent %d _
		noa wincmd p
    else
		exe 'topleft ' . s:nrrw_rgn_wdth . (s:nrrw_rgn_vert?'v':'') . "sp " . s:nrrw_winname
		setl noswapfile buftype=acwrite bufhidden=wipe foldcolumn=0 nobuflisted
		let nrrw_win = bufwinnr("")
    endif
    return nrrw_win
endfu

fun! nrrwrgn#NrrwRgn() range  "{{{1
	let o_lz = &lz
	let s:o_s  = @/
	set lz
	let orig_buf=bufnr('')

	" initialize Variables
	call <sid>Init()
	" Protect the original buffer,
	" so you won't accidentally modify those lines,
	" that might later be overwritten
	setl noma
	let ft=&l:ft
	let s:nrrw_rgn_lines[s:instn].startline = [ a:firstline, 0 ]
	let s:nrrw_rgn_lines[s:instn].endline   = [ a:lastline, 0 ]
	if exists("s:nrrw_rgn_lines[s:instn].matchid")
	    " if you call :NarrowRegion several times, without widening 
	    " the previous region, b:matchid might already be defined so
	    " make sure, the previous highlighting is removed.
	    call matchdelete(s:nrrw_rgn_lines[s:instn].matchid)
	endif
	if !s:nrrw_rgn_nohl
	    let s:nrrw_rgn_lines[s:instn].matchid =  matchadd(s:nrrw_rgn_hl, 
		\<sid>GeneratePattern(
		\s:nrrw_rgn_lines[s:instn].startline, 
		\s:nrrw_rgn_lines[s:instn].endline, 
		\'V')) "set the highlighting
	endif
	let a=getline(
	    \s:nrrw_rgn_lines[s:instn].startline[0], 
	    \s:nrrw_rgn_lines[s:instn].endline[0])
	let win=<sid>NrwRgnWin()
	exe ':noa ' win 'wincmd w'
	let b:orig_buf = orig_buf
	call setline(1, a)
	let b:nrrw_instn = s:instn
	setl nomod
	"com! -buffer WidenRegion :call nrrwrgn#WidenRegion(0) |sil bd!
    com! -buffer -bang WidenRegion :call nrrwrgn#WidenRegion(0, (empty("<bang>") ? 0 : 1))
	call <sid>NrrwRgnAuCmd(0)

	" restore settings
	let &l:ft = ft
	let &lz   = o_lz
endfun

fu! s:WriteNrrwRgn(...) "{{{1
	if (bufwinnr(b:orig_buf) == -1)
		call s:WarningMsg("Original buffer does no longer exist! Aborting!")
		return
	endif
    if &l:mod && exists("a:1") && a:1
		" Write the buffer back to the original buffer
		setl nomod
		exe ":WidenRegion"
    else
		call setbufvar(b:orig_buf, '&ma', 1)
		exe ':noa' . bufwinnr(b:orig_buf) . 'wincmd w'
		"if exists("s:nrrw_rgn_lines[s:instn].matchid")
		"	call matchdelete(s:nrrw_rgn_lines[s:instn].matchid)
		"	unlet s:nrrw_rgn_lines[s:instn].matchid
		"endif
    endif
endfun

fu! nrrwrgn#WidenRegion(vmode,force) "{{{1
    let nrw_buf  = bufnr('')
    let orig_win = bufwinnr(b:orig_buf)
	if (orig_win == -1)
		call s:WarningMsg("Original buffer does no longer exist! Aborting!")
		return
	endif
    let cont     = getline(1,'$')
	let instn    = b:nrrw_instn
    call <sid>SaveRestoreRegister(1)
    exe ':noa' . orig_win . 'wincmd w'
	let wsv=winsaveview()
    if !(&l:ma)
		setl ma
    endif
    if a:vmode "charwise, linewise or blockwise selection 
		call setreg('a', join(cont, "\n") . "\n", s:nrrw_rgn_lines[instn].vmode)
		if s:nrrw_rgn_lines[instn].vmode == 'v'
		   " in characterwise selection, remove trailing \n
		   call setreg('a', substitute(@a, '\n$', '', ''), 
			   \s:nrrw_rgn_lines[instn].vmode)
		endif
		exe "keepj" s:nrrw_rgn_lines[instn].startline[0]
		exe "keepj norm!" s:nrrw_rgn_lines[instn].startline[1] . '|'
		exe "keepj norm!" s:nrrw_rgn_lines[instn].vmode
		exe "keepj" s:nrrw_rgn_lines[instn].endline[0]
		exe "keepj norm!" s:nrrw_rgn_lines[instn].endline[1] . '|'
		norm! "aP
		" Recalculate the start and end positions of the narrowed window
		" so subsequent calls will adjust the region accordingly
		let [ s:nrrw_rgn_lines[instn].startline, 
			 \s:nrrw_rgn_lines[instn].endline ] = <sid>RetVisRegionPos()
		" also, renew the highlighted region
		if exists("s:nrrw_rgn_lines[instn].matchid")
			" if you call :NarrowRegion several times, without widening 
			" the previous region, b:matchid might already be defined so
			" make sure, the previous highlighting is removed.
			call matchdelete(s:nrrw_rgn_lines[instn].matchid)
		endif
		if !s:nrrw_rgn_nohl
			let s:nrrw_rgn_lines[instn].matchid =  matchadd(s:nrrw_rgn_hl, 
			\<sid>GeneratePattern(
			\s:nrrw_rgn_lines[instn].startline, 
			\s:nrrw_rgn_lines[instn].endline, 
			\s:nrrw_rgn_lines[instn].vmode))
		endif
    else "linewise selection because we started the NarrowRegion with the command NarrowRegion(0)
		"
		" if the endposition of the narrowed buffer is also the last line of
		" the buffer, the append will add an extra newline that needs to be
		" cleared.
		if s:nrrw_rgn_lines[instn].endline[0]==line('$') &&
		\  s:nrrw_rgn_lines[instn].startline[0] == 1
			let delete_last_line=1
		else
			let delete_last_line=0
		endif
		exe ':silent :'.s:nrrw_rgn_lines[instn].startline[0].','
			\.s:nrrw_rgn_lines[instn].endline[0].'d _'
		call append((s:nrrw_rgn_lines[instn].startline[0]-1),cont)
		" Recalculate the start and end positions of the narrowed window
		" so subsequent calls will adjust the region accordingly
		" so subsequent calls will adjust the region accordingly
	    let  s:nrrw_rgn_lines[instn].endline[0] =
			\s:nrrw_rgn_lines[instn].startline[0] + len(cont) -1
		if s:nrrw_rgn_lines[instn].endline[0] > line('$')
			let s:nrrw_rgn_lines[instn].endline[0] = line('$')
		endif
		" also, renew the highlighted region
		if exists("s:nrrw_rgn_lines[instn].matchid")
			" if you call :NarrowRegion several times, without widening 
			" the previous region, b:matchid might already be defined so
			" make sure, the previous highlighting is removed.
			call matchdelete(s:nrrw_rgn_lines[instn].matchid)
		endif
		if !s:nrrw_rgn_nohl
			let s:nrrw_rgn_lines[instn].matchid =  matchadd(s:nrrw_rgn_hl, 
			\<sid>GeneratePattern(
			\s:nrrw_rgn_lines[instn].startline, 
			\s:nrrw_rgn_lines[instn].endline, 
			\'V'))
		endif
	    if delete_last_line
			:$d _
	    endif
    endif
    call <sid>SaveRestoreRegister(0)
    let  @/=s:o_s
	call winrestview(wsv)
    " jump back to narrowed window
    exe ':noa' . bufwinnr(nrw_buf) . 'wincmd w'
    "call <sid>NrrwRgnAuCmd(0)
	"exe ':silent :bd!' nrw_buf
	setl nomod
	if a:force
		"exe 'bd! ' nrw_buf
		:bd!
	endif
endfu

fu! <sid>SaveRestoreRegister(mode) "{{{1
    if a:mode
		let s:savereg  = getreg('a')
		let s:saveregt = getregtype('a')
    else
		call setreg('a', s:savereg, s:saveregt)
    endif
endfu!

fu! nrrwrgn#VisualNrrwRgn(mode) "{{{1
    if empty(a:mode)
		" in case, visual mode wasn't entered, visualmode()
		" returns an empty string and in that case, we finish
		" here
		call <sid>WarningMsg("There was no region visually selected!")
		return
    endif
    " This beeps, when called from command mode
    " e.g. by using :NRV, so using :sil!
    " else exiting visual mode
    exe "sil! norm! \<ESC>"
    " stop visualmode
    let o_lz = &lz
    let s:o_s  = @/
    set lz
    call <sid>Init()
    let s:nrrw_rgn_lines[s:instn].vmode=a:mode
    " Protect the original buffer,
    " so you won't accidentally modify those lines,
    " that will later be overwritten
    setl noma
    let orig_buf=bufnr('')
    call <sid>SaveRestoreRegister(1)

    let ft=&l:ft
    let [ s:nrrw_rgn_lines[s:instn].startline, s:nrrw_rgn_lines[s:instn].endline ] = <sid>RetVisRegionPos()
    if exists("s:nrrw_rgn_lines[s:instn].matchid")
		" if you call :NarrowRegion several times, without widening 
		" the previous region, b:matchid might already be defined so
		" make sure, the previous highlighting is removed.
		call matchdelete(s:nrrw_rgn_lines[s:instn].matchid)
    endif
    if !s:nrrw_rgn_nohl
		let s:nrrw_rgn_lines[s:instn].matchid =  matchadd(s:nrrw_rgn_hl, 
		\<sid>GeneratePattern(s:nrrw_rgn_lines[s:instn].startline, s:nrrw_rgn_lines[s:instn].endline, s:nrrw_rgn_lines[s:instn].vmode))
    endif
    norm gv"ay
    let win=<sid>NrwRgnWin()
    exe ':noa ' win 'wincmd w'
    let b:orig_buf = orig_buf
    silent put a
	let b:nrrw_instn = s:instn
    silent 0d _
    setl nomod
    "com! -buffer WidenRegion :call nrrwrgn#WidenRegion(1)|sil bd!
    com! -buffer -bang WidenRegion :call nrrwrgn#WidenRegion(1, (empty("<bang>") ? 0 : 1))
    call <sid>NrrwRgnAuCmd(0)
    call <sid>SaveRestoreRegister(0)

    " restore settings
    let &l:ft = ft
    let &lz   = o_lz
endfu

fu! <sid>NrrwRgnAuCmd(bufnr) "{{{1
	" If a:bufnr==0, then enable auto commands
	" else disable auto commands for a:bufnr
    if !a:bufnr
		exe "aug NrrwRgn" . b:nrrw_instn
			au!
			au BufWriteCmd <buffer> nested :call s:WriteNrrwRgn(1)
			exe "au BufWipeout,BufDelete <buffer> nested :call s:WriteNrrwRgn()|:call <sid>NrrwRgnAuCmd(".b:nrrw_instn.")"
		aug end
    else
		exe "aug NrrwRgn" .  a:bufnr
		au!
		aug end
		exe "aug! NrrwRgn" . a:bufnr
		if exists("s:nrrw_rgn_lines[a:bufnr].matchid")
			call matchdelete(s:nrrw_rgn_lines[a:bufnr].matchid)
			unlet s:nrrw_rgn_lines[a:bufnr].matchid
		endif
		if s:instn>0
			unlet s:nrrw_rgn_lines[a:bufnr]
			let s:instn-=1
		endif
    endif
endfun

fu! <sid>RetVisRegionPos() "{{{1
    let startline = [ getpos("'<")[1], virtcol("'<") ]
    let endline   = [ getpos("'>")[1], virtcol("'>") ]
    return [ startline, endline ]
endfu

fun! <sid>GeneratePattern(startl, endl, mode) "{{{1
    if a:mode ==# ''
	return '\%>' . (a:startl[0]-1) . 'l\&\%>' . (a:startl[1]-1) . 'v\&\%<' . (a:endl[0]+1) . 'l\&\%<' . (a:endl[1]+1) . 'v'
    elseif a:mode ==# 'v'
	return '\%>' . (a:startl[0]-1) . 'l\&\%>' . (a:startl[1]-1) . 'v\_.*\%<' . (a:endl[0]+1) . 'l\&\%<' . (a:endl[1]+1) . 'v'
    else
	return '\%>' . (a:startl[0]-1) . 'l\&\%<' . (a:endl[0]+1) . 'l'
    endif
endfun "}}}

" vim: ts=4 sts=4 fdm=marker com+=l\:\" fdl=0
doc/NarrowRegion.txt	[[[1
227
*NrrwRgn.txt*   A Narrow Region Plugin (similar to Emacs)

Author:  Christian Brabandt <cb@256bit.org>
Version: 0.11 Wed, 28 Jul 2010 20:33:05 +0200

Copyright: (c) 2009, 2010 by Christian Brabandt         
           The VIM LICENSE applies to NrrwRgnPlugin.vim and NrrwRgnPlugin.txt
           (see |copyright|) except use NrrwRgnPlugin instead of "Vim".
           NO WARRANTY, EXPRESS OR IMPLIED.  USE AT-YOUR-OWN-RISK.


==============================================================================
1. Contents                                     *NarrowRegion*  *NrrwRgnPlugin*

        1.  Contents.....................................: |NrrwRgnPlugin|
        2.  NrrwRgn Manual...............................: |NrrwRgn-manual|
        2.1   NrrwRgn Configuration......................: |NrrwRgn-config|
        3.  NrrwRgn Feedback.............................: |NrrwRgn-feedback|
        4.  NrrwRgn History..............................: |NrrwRgn-history|

==============================================================================
2. NrrwRgn Manual                                       *NrrwRgn-manual*

Functionality

This plugin is based on a discussion in comp.editors (see the thread at
http://groups.google.com/group/comp.editors/browse_frm/thread/0f562d97f80dde13)

Narrowing means focussing on a region and making the rest inaccessible. You
simply select the region, call :NarrowRegion and the selected part will open
in a new scratch buffer. The rest of the file will be protected, so you won't
accidentally modify that buffer. In the new buffer, you can do a global
replace, search or anything else to modify that part. When you are finished,
simply write that buffer (e.g. by |:w|) and your modifications will be put in
the original buffer making it accessible again.

NrrwRgn allows you to either select a line based selection using an Ex-command
or you can simply use any visual selected region and press your prefered key
combination to open that selection in a new buffer.

                                                        *:NarrowRegion* *:NR*
:[range]NarrowRegion        When [range] is omited, select only the current
                            line, else use the lines in the range given and 
                            open it in a new Scratch Window. 
                            Whenever you are finished modifying that region
                            simply write the buffer.

:[range]NR                  This is a shortcut for :NarrowRegion

                                                        *:NarrowWindow* *:NW*
:NarrowWindow               Select only the range that is visible the current
                            window and open it in a new Scratch Window. 
                            Whenever you are finished modifying that region
                            simply write the buffer.

:NW                         This is a shortcut for :NarrowWindow

                                                                *:WidenRegion*
:WidenRegion[!]             This command is only available in the narrowed 
                            scratch window. If the buffer has been modified,
                            the contents will be put back on the original
                            buffer. If ! is specified, the window will be
                            closed, otherwise it will remain open.
                                                                
                                                                        *:NRV*
:NRW                        Opened the narrowed window for the region that was
                            last selected in visual mode

You can also start visual mode and have the selected region being narrowed. In
this mode, NarrowRegion allows you to block select |CTRL-V| , character select
|v| or linewise select |V| a region. Then press <Leader>nr where <Leader> by
default is set to '\', unless you have set it to something different (see
|<Leader>| for information how to change this) and the selected range will
open in a new scratch buffer. This key combination only works in |Visual-mode|

When finished, simply write that Narrowed Region window, from which you want
to take the modifications in your original file. 

It is possible, to recursively open a Narrowed Window on top of an already
narrowed window. This sounds a little bit silly, but this makes it possible,
to have several narrowed windows, which you can use for several different
things, e.g. If you have 2 different buffers opened and you want to diff a
certain region of each of those 2 buffers, simply open a Narrowed Window for
each buffer, and execute |:diffthis| in each narrowed window. 

You can then interactively merge those 2 windows. And when you are finished,
simply write the narrowed window and the changes will be taken back into the
original buffer.
==============================================================================
2.1 NrrwRgn Configuration                                    *NrrwRgn-config*

NarrowRegion can be customized by setting some global variables. If you'd
like to open the narrowed windo as a vertical split buffer, simply set the
variable g:nrrw_rgn_vert to 1 in your |.vimrc| >

    let g:nrrw_rgn_vert = 1
<
------------------------------------------------------------------------------

If you'd like to specify a certain width/height for you scratch buffer, then
set the variable g:nrrw_rgn_wdth in your |.vimrc| . This variable defines the
width or the nr of columns, if you have also set g:nrrw_rgn_vert. >

    let g:nrrw_rgn_wdth = 30
<
------------------------------------------------------------------------------

By default, NarrowRegion highlights the region that has been selected
using the WildMenu highlighting (see |hl-WildMenu|). If you'd like to use a
different highlighting, set the variable g:nrrw_rgn_hl to your preferred
highlighting Group. For example to have the region highlighted like a search
result, you could put that in your |.vimrc| >

    let g:nrrw_rgn_hl = 'Search'
<
If you want to turn off the highlighting (because this can be disturbing, you
can set the global variable g:nrrw_rgn_nohl to 1 in your |.vimrc| >

    let g:nrrw_rgn_nohl = 1
<
------------------------------------------------------------------------------

If you'd like to change the key combination, that starts the Narrowed Window
for you selected range, you could put this in your |.vimrc| >

   xmap <F3> <Plug>NrrwrgnDo
<
This will let <F3> open the Narrow-Window, but only if you have pressed it in
Visual Mode. It doesn't really make sense to map this combination to any other
mode, unless you want it to Narrow your last visually selected range.

==============================================================================
3. NrrwRgn Feedback                                         *NrrwRgn-feedback*

Feedback is always welcome. If you like the plugin, please rate it at the
vim-page:
http://www.vim.org/scripts/script.php?script_id=3075

You can also follow the development of the plugin at github:
http://github.com/chrisbra/NrrwRgn

Please don't hesitate to report any bugs to the maintainer, mentioned in the
third line of this document.

==============================================================================
4. NrrwRgn History                                          *NrrwRgn-history*

0.11: July 28, 2010

- Don't set 'winfixwidth' and 'winfixheight' (suggested by Charles Campbell)

0.10: May 20,2010

- Restore Cursorposition using winrestview() and winsaveview()
- fix a bug, that prevented the use of visual narrowing
- Make sure when closing the narrowed buffer, the content will be written to
  the right original region
- use topleft for opening the Narrowed window
- check, that the original buffer is still available
- If you Narrow the complete buffer using :NRV and write the changes back, an
  additional trailing line is inserted. Remove that line.
- When writing the changes back, update the highlighting.

0.9: May 20, 2010

- It is now possible to Narrow a window recursively. This allows to have
  several narrowed windows, and allows for example to only diff certain
  regions (as was suggested in a recent thread at the vim_use mailinglist:
  http://groups.google.com/group/vim_use/msg/05d7fd9bd1556f0e) therefore, the
  use for the g:nrrw_rgn_sepwin variable isn't necessary anymore.
- Small documentation updates

0.8: May 18, 2010

- the g:nrrw_rgn_sepwin variable can be used to force seperate Narrowed
  Windows, so you could easily diff those windows.
- make the separating of several windows a little bit safer (look at the
  bufnr(), so it should work without problems for several buffers)
- switch from script local variables to buffer local variables, so narrowing
  for several buffers should work.
- set 'winfixheight' for narrowed window 
- Added command :NRV (suggested by Charles Campbell, thanks!)
- added error handling, in case :NRV is called, without a selected region
- take care of beeps, when calling :NRV
- output WarningMsg

0.7: May 17, 2010

- really use the black hole register for deleting the old buffer contents in
  the narrowed buffer (suggestion by esquifit in
  http://groups.google.com/group/comp.editors/msg/3eb3e3a7c68597db)
- make autocommand nesting, so the highlighting will be removed when writing
  the buffer contents.
- Use g:nrrw_rgn_nohl variable to disable highlighting (as this can be
  disturbing).

0.6: May 04, 2010

- the previous version had problems restoring the orig buffer, this version
  fixes it (highlighting and setl ma did not work correctly)

0.5: May 04, 2010       

- The mapping that allows for narrowing a visually selected range, did not
  work.  (Fixed!)
- Make :WidenRegion work as expected (close the widened window) (unreleased)

0.4: Apr 28, 2010       

- Highlight narrowed region in the original buffer
- Save and Restore search-register
- Provide shortcut commands |:NR| 
- Provide command |:NW| and |:NarrowWindow|
- Make plugin autoloadable
- Enable GLVS (see |:GLVS|)
- Provide Documenation (:h NarrowRegion)
- Distribute Plugin as vimball |pi_vimball.txt|

0.3: Apr 28, 2010       

- Initial upload
- development versions are available at the github repository
- put plugin on a public repository (http://github.com/chrisbra/NrrwRgn)

==============================================================================
Modeline:
vim:tw=78:ts=8:ft=help:et
</span>
<span id="LID177" rel="#L177">177</span>
<span id="LID178" rel="#L178">178</span>
<span id="LID179" rel="#L179">179</span>
<span id="LID180" rel="#L180">180</span>
<span id="LID181" rel="#L181">181</span>
<span id="LID182" rel="#L182">182</span>
<span id="LID183" rel="#L183">183</span>
<span id="LID184" rel="#L184">184</span>
<span id="LID185" rel="#L185">185</span>
<span id="LID186" rel="#L186">186</span>
<span id="LID187" rel="#L187">187</span>
<span id="LID188" rel="#L188">188</span>
<span id="LID189" rel="#L189">189</span>
<span id="LID190" rel="#L190">190</span>
<span id="LID191" rel="#L191">191</span>
<span id="LID192" rel="#L192">192</span>
<span id="LID193" rel="#L193">193</span>
<span id="LID194" rel="#L194">194</span>
<span id="LID195" rel="#L195">195</span>
<span id="LID196" rel="#L196">196</span>
<span id="LID197" rel="#L197">197</span>
<span id="LID198" rel="#L198">198</span>
<span id="LID199" rel="#L199">199</span>
<span id="LID200" rel="#L200">200</span>
<span id="LID201" rel="#L201">201</span>
<span id="LID202" rel="#L202">202</span>
<span id="LID203" rel="#L203">203</span>
<span id="LID204" rel="#L204">204</span>
<span id="LID205" rel="#L205">205</span>
<span id="LID206" rel="#L206">206</span>
<span id="LID207" rel="#L207">207</span>
<span id="LID208" rel="#L208">208</span>
<span id="LID209" rel="#L209">209</span>
<span id="LID210" rel="#L210">210</span>
<span id="LID211" rel="#L211">211</span>
<span id="LID212" rel="#L212">212</span>
<span id="LID213" rel="#L213">213</span>
<span id="LID214" rel="#L214">214</span>
<span id="LID215" rel="#L215">215</span>
<span id="LID216" rel="#L216">216</span>
<span id="LID217" rel="#L217">217</span>
<span id="LID218" rel="#L218">218</span>
<span id="LID219" rel="#L219">219</span>
<span id="LID220" rel="#L220">220</span>
<span id="LID221" rel="#L221">221</span>
<span id="LID222" rel="#L222">222</span>
<span id="LID223" rel="#L223">223</span>
<span id="LID224" rel="#L224">224</span>
<span id="LID225" rel="#L225">225</span>
<span id="LID226" rel="#L226">226</span>
<span id="LID227" rel="#L227">227</span>
<span id="LID228" rel="#L228">228</span>
<span id="LID229" rel="#L229">229</span>
<span id="LID230" rel="#L230">230</span>
<span id="LID231" rel="#L231">231</span>
<span id="LID232" rel="#L232">232</span>
<span id="LID233" rel="#L233">233</span>
<span id="LID234" rel="#L234">234</span>
<span id="LID235" rel="#L235">235</span>
<span id="LID236" rel="#L236">236</span>
<span id="LID237" rel="#L237">237</span>
<span id="LID238" rel="#L238">238</span>
<span id="LID239" rel="#L239">239</span>
<span id="LID240" rel="#L240">240</span>
<span id="LID241" rel="#L241">241</span>
<span id="LID242" rel="#L242">242</span>
<span id="LID243" rel="#L243">243</span>
<span id="LID244" rel="#L244">244</span>
<span id="LID245" rel="#L245">245</span>
<span id="LID246" rel="#L246">246</span>
<span id="LID247" rel="#L247">247</span>
<span id="LID248" rel="#L248">248</span>
<span id="LID249" rel="#L249">249</span>
<span id="LID250" rel="#L250">250</span>
<span id="LID251" rel="#L251">251</span>
<span id="LID252" rel="#L252">252</span>
<span id="LID253" rel="#L253">253</span>
<span id="LID254" rel="#L254">254</span>
<span id="LID255" rel="#L255">255</span>
<span id="LID256" rel="#L256">256</span>
<span id="LID257" rel="#L257">257</span>
<span id="LID258" rel="#L258">258</span>
<span id="LID259" rel="#L259">259</span>
<span id="LID260" rel="#L260">260</span>
<span id="LID261" rel="#L261">261</span>
<span id="LID262" rel="#L262">262</span>
<span id="LID263" rel="#L263">263</span>
<span id="LID264" rel="#L264">264</span>
<span id="LID265" rel="#L265">265</span>
<span id="LID266" rel="#L266">266</span>
<span id="LID267" rel="#L267">267</span>
<span id="LID268" rel="#L268">268</span>
<span id="LID269" rel="#L269">269</span>
<span id="LID270" rel="#L270">270</span>
<span id="LID271" rel="#L271">271</span>
<span id="LID272" rel="#L272">272</span>
<span id="LID273" rel="#L273">273</span>
<span id="LID274" rel="#L274">274</span>
<span id="LID275" rel="#L275">275</span>
<span id="LID276" rel="#L276">276</span>
<span id="LID277" rel="#L277">277</span>
<span id="LID278" rel="#L278">278</span>
<span id="LID279" rel="#L279">279</span>
<span id="LID280" rel="#L280">280</span>
<span id="LID281" rel="#L281">281</span>
<span id="LID282" rel="#L282">282</span>
<span id="LID283" rel="#L283">283</span>
<span id="LID284" rel="#L284">284</span>
<span id="LID285" rel="#L285">285</span>
<span id="LID286" rel="#L286">286</span>
<span id="LID287" rel="#L287">287</span>
<span id="LID288" rel="#L288">288</span>
<span id="LID289" rel="#L289">289</span>
<span id="LID290" rel="#L290">290</span>
<span id="LID291" rel="#L291">291</span>
<span id="LID292" rel="#L292">292</span>
<span id="LID293" rel="#L293">293</span>
<span id="LID294" rel="#L294">294</span>
<span id="LID295" rel="#L295">295</span>
<span id="LID296" rel="#L296">296</span>
<span id="LID297" rel="#L297">297</span>
<span id="LID298" rel="#L298">298</span>
<span id="LID299" rel="#L299">299</span>
<span id="LID300" rel="#L300">300</span>
<span id="LID301" rel="#L301">301</span>
<span id="LID302" rel="#L302">302</span>
<span id="LID303" rel="#L303">303</span>
<span id="LID304" rel="#L304">304</span>
<span id="LID305" rel="#L305">305</span>
<span id="LID306" rel="#L306">306</span>
<span id="LID307" rel="#L307">307</span>
<span id="LID308" rel="#L308">308</span>
<span id="LID309" rel="#L309">309</span>
<span id="LID310" rel="#L310">310</span>
<span id="LID311" rel="#L311">311</span>
<span id="LID312" rel="#L312">312</span>
<span id="LID313" rel="#L313">313</span>
<span id="LID314" rel="#L314">314</span>
<span id="LID315" rel="#L315">315</span>
<span id="LID316" rel="#L316">316</span>
<span id="LID317" rel="#L317">317</span>
<span id="LID318" rel="#L318">318</span>
<span id="LID319" rel="#L319">319</span>
<span id="LID320" rel="#L320">320</span>
<span id="LID321" rel="#L321">321</span>
<span id="LID322" rel="#L322">322</span>
<span id="LID323" rel="#L323">323</span>
<span id="LID324" rel="#L324">324</span>
<span id="LID325" rel="#L325">325</span>
<span id="LID326" rel="#L326">326</span>
<span id="LID327" rel="#L327">327</span>
<span id="LID328" rel="#L328">328</span>
<span id="LID329" rel="#L329">329</span>
<span id="LID330" rel="#L330">330</span>
<span id="LID331" rel="#L331">331</span>
<span id="LID332" rel="#L332">332</span>
<span id="LID333" rel="#L333">333</span>
<span id="LID334" rel="#L334">334</span>
<span id="LID335" rel="#L335">335</span>
<span id="LID336" rel="#L336">336</span>
<span id="LID337" rel="#L337">337</span>
<span id="LID338" rel="#L338">338</span>
<span id="LID339" rel="#L339">339</span>
<span id="LID340" rel="#L340">340</span>
<span id="LID341" rel="#L341">341</span>
<span id="LID342" rel="#L342">342</span>
<span id="LID343" rel="#L343">343</span>
<span id="LID344" rel="#L344">344</span>
<span id="LID345" rel="#L345">345</span>
<span id="LID346" rel="#L346">346</span>
<span id="LID347" rel="#L347">347</span>
<span id="LID348" rel="#L348">348</span>
<span id="LID349" rel="#L349">349</span>
<span id="LID350" rel="#L350">350</span>
<span id="LID351" rel="#L351">351</span>
<span id="LID352" rel="#L352">352</span>
<span id="LID353" rel="#L353">353</span>
<span id="LID354" rel="#L354">354</span>
<span id="LID355" rel="#L355">355</span>
<span id="LID356" rel="#L356">356</span>
<span id="LID357" rel="#L357">357</span>
<span id="LID358" rel="#L358">358</span>
<span id="LID359" rel="#L359">359</span>
<span id="LID360" rel="#L360">360</span>
<span id="LID361" rel="#L361">361</span>
<span id="LID362" rel="#L362">362</span>
<span id="LID363" rel="#L363">363</span>
<span id="LID364" rel="#L364">364</span>
<span id="LID365" rel="#L365">365</span>
<span id="LID366" rel="#L366">366</span>
<span id="LID367" rel="#L367">367</span>
<span id="LID368" rel="#L368">368</span>
<span id="LID369" rel="#L369">369</span>
<span id="LID370" rel="#L370">370</span>
<span id="LID371" rel="#L371">371</span>
<span id="LID372" rel="#L372">372</span>
<span id="LID373" rel="#L373">373</span>
<span id="LID374" rel="#L374">374</span>
<span id="LID375" rel="#L375">375</span>
<span id="LID376" rel="#L376">376</span>
<span id="LID377" rel="#L377">377</span>
<span id="LID378" rel="#L378">378</span>
<span id="LID379" rel="#L379">379</span>
<span id="LID380" rel="#L380">380</span>
<span id="LID381" rel="#L381">381</span>
<span id="LID382" rel="#L382">382</span>
<span id="LID383" rel="#L383">383</span>
<span id="LID384" rel="#L384">384</span>
<span id="LID385" rel="#L385">385</span>
<span id="LID386" rel="#L386">386</span>
<span id="LID387" rel="#L387">387</span>
<span id="LID388" rel="#L388">388</span>
<span id="LID389" rel="#L389">389</span>
<span id="LID390" rel="#L390">390</span>
<span id="LID391" rel="#L391">391</span>
<span id="LID392" rel="#L392">392</span>
<span id="LID393" rel="#L393">393</span>
<span id="LID394" rel="#L394">394</span>
<span id="LID395" rel="#L395">395</span>
<span id="LID396" rel="#L396">396</span>
<span id="LID397" rel="#L397">397</span>
<span id="LID398" rel="#L398">398</span>
<span id="LID399" rel="#L399">399</span>
<span id="LID400" rel="#L400">400</span>
<span id="LID401" rel="#L401">401</span>
<span id="LID402" rel="#L402">402</span>
<span id="LID403" rel="#L403">403</span>
<span id="LID404" rel="#L404">404</span>
<span id="LID405" rel="#L405">405</span>
<span id="LID406" rel="#L406">406</span>
<span id="LID407" rel="#L407">407</span>
<span id="LID408" rel="#L408">408</span>
<span id="LID409" rel="#L409">409</span>
<span id="LID410" rel="#L410">410</span>
<span id="LID411" rel="#L411">411</span>
<span id="LID412" rel="#L412">412</span>
<span id="LID413" rel="#L413">413</span>
<span id="LID414" rel="#L414">414</span>
<span id="LID415" rel="#L415">415</span>
<span id="LID416" rel="#L416">416</span>
<span id="LID417" rel="#L417">417</span>
<span id="LID418" rel="#L418">418</span>
<span id="LID419" rel="#L419">419</span>
<span id="LID420" rel="#L420">420</span>
<span id="LID421" rel="#L421">421</span>
<span id="LID422" rel="#L422">422</span>
<span id="LID423" rel="#L423">423</span>
<span id="LID424" rel="#L424">424</span>
<span id="LID425" rel="#L425">425</span>
<span id="LID426" rel="#L426">426</span>
<span id="LID427" rel="#L427">427</span>
<span id="LID428" rel="#L428">428</span>
<span id="LID429" rel="#L429">429</span>
<span id="LID430" rel="#L430">430</span>
<span id="LID431" rel="#L431">431</span>
<span id="LID432" rel="#L432">432</span>
<span id="LID433" rel="#L433">433</span>
<span id="LID434" rel="#L434">434</span>
<span id="LID435" rel="#L435">435</span>
<span id="LID436" rel="#L436">436</span>
<span id="LID437" rel="#L437">437</span>
<span id="LID438" rel="#L438">438</span>
<span id="LID439" rel="#L439">439</span>
<span id="LID440" rel="#L440">440</span>
<span id="LID441" rel="#L441">441</span>
<span id="LID442" rel="#L442">442</span>
<span id="LID443" rel="#L443">443</span>
<span id="LID444" rel="#L444">444</span>
<span id="LID445" rel="#L445">445</span>
<span id="LID446" rel="#L446">446</span>
<span id="LID447" rel="#L447">447</span>
<span id="LID448" rel="#L448">448</span>
<span id="LID449" rel="#L449">449</span>
<span id="LID450" rel="#L450">450</span>
<span id="LID451" rel="#L451">451</span>
<span id="LID452" rel="#L452">452</span>
<span id="LID453" rel="#L453">453</span>
<span id="LID454" rel="#L454">454</span>
<span id="LID455" rel="#L455">455</span>
<span id="LID456" rel="#L456">456</span>
<span id="LID457" rel="#L457">457</span>
<span id="LID458" rel="#L458">458</span>
<span id="LID459" rel="#L459">459</span>
<span id="LID460" rel="#L460">460</span>
<span id="LID461" rel="#L461">461</span>
<span id="LID462" rel="#L462">462</span>
<span id="LID463" rel="#L463">463</span>
<span id="LID464" rel="#L464">464</span>
<span id="LID465" rel="#L465">465</span>
<span id="LID466" rel="#L466">466</span>
<span id="LID467" rel="#L467">467</span>
<span id="LID468" rel="#L468">468</span>
<span id="LID469" rel="#L469">469</span>
<span id="LID470" rel="#L470">470</span>
<span id="LID471" rel="#L471">471</span>
<span id="LID472" rel="#L472">472</span>
<span id="LID473" rel="#L473">473</span>
<span id="LID474" rel="#L474">474</span>
<span id="LID475" rel="#L475">475</span>
<span id="LID476" rel="#L476">476</span>
<span id="LID477" rel="#L477">477</span>
<span id="LID478" rel="#L478">478</span>
<span id="LID479" rel="#L479">479</span>
<span id="LID480" rel="#L480">480</span>
<span id="LID481" rel="#L481">481</span>
<span id="LID482" rel="#L482">482</span>
<span id="LID483" rel="#L483">483</span>
<span id="LID484" rel="#L484">484</span>
<span id="LID485" rel="#L485">485</span>
<span id="LID486" rel="#L486">486</span>
<span id="LID487" rel="#L487">487</span>
<span id="LID488" rel="#L488">488</span>
<span id="LID489" rel="#L489">489</span>
<span id="LID490" rel="#L490">490</span>
<span id="LID491" rel="#L491">491</span>
<span id="LID492" rel="#L492">492</span>
<span id="LID493" rel="#L493">493</span>
<span id="LID494" rel="#L494">494</span>
<span id="LID495" rel="#L495">495</span>
<span id="LID496" rel="#L496">496</span>
<span id="LID497" rel="#L497">497</span>
<span id="LID498" rel="#L498">498</span>
<span id="LID499" rel="#L499">499</span>
<span id="LID500" rel="#L500">500</span>
<span id="LID501" rel="#L501">501</span>
<span id="LID502" rel="#L502">502</span>
<span id="LID503" rel="#L503">503</span>
<span id="LID504" rel="#L504">504</span>
<span id="LID505" rel="#L505">505</span>
<span id="LID506" rel="#L506">506</span>
<span id="LID507" rel="#L507">507</span>
<span id="LID508" rel="#L508">508</span>
<span id="LID509" rel="#L509">509</span>
<span id="LID510" rel="#L510">510</span>
<span id="LID511" rel="#L511">511</span>
<span id="LID512" rel="#L512">512</span>
<span id="LID513" rel="#L513">513</span>
<span id="LID514" rel="#L514">514</span>
<span id="LID515" rel="#L515">515</span>
<span id="LID516" rel="#L516">516</span>
<span id="LID517" rel="#L517">517</span>
<span id="LID518" rel="#L518">518</span>
<span id="LID519" rel="#L519">519</span>
<span id="LID520" rel="#L520">520</span>
<span id="LID521" rel="#L521">521</span>
<span id="LID522" rel="#L522">522</span>
<span id="LID523" rel="#L523">523</span>
<span id="LID524" rel="#L524">524</span>
<span id="LID525" rel="#L525">525</span>
<span id="LID526" rel="#L526">526</span>
<span id="LID527" rel="#L527">527</span>
<span id="LID528" rel="#L528">528</span>
<span id="LID529" rel="#L529">529</span>
<span id="LID530" rel="#L530">530</span>
<span id="LID531" rel="#L531">531</span>
<span id="LID532" rel="#L532">532</span>
<span id="LID533" rel="#L533">533</span>
<span id="LID534" rel="#L534">534</span>
<span id="LID535" rel="#L535">535</span>
<span id="LID536" rel="#L536">536</span>
<span id="LID537" rel="#L537">537</span>
<span id="LID538" rel="#L538">538</span>
<span id="LID539" rel="#L539">539</span>
<span id="LID540" rel="#L540">540</span>
<span id="LID541" rel="#L541">541</span>
<span id="LID542" rel="#L542">542</span>
</pre>
          </td>
          <td width="100%">
            
              <div class="highlight"><pre><div class='line' id='LC1'>" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.</div><div class='line' id='LC2'>UseVimball</div><div class='line' id='LC3'>finish</div><div class='line' id='LC4'>plugin/NrrwRgn.vim	[[[1</div><div class='line' id='LC5'>44</div><div class='line' id='LC6'>" NrrwRgn.vim - Narrow Region plugin for Vim</div><div class='line' id='LC7'>" -------------------------------------------------------------</div><div class='line' id='LC8'>" Version:	   0.9</div><div class='line' id='LC9'>" Maintainer:  Christian Brabandt &lt;cb@256bit.org&gt;</div><div class='line' id='LC10'>" Last Change: Thu, 20 May 2010 08:08:50 +0200</div><div class='line' id='LC11'>"</div><div class='line' id='LC12'>" Script: http://www.vim.org/scripts/script.php?script_id=3075 </div><div class='line' id='LC13'>" Copyright:   (c) 2009, 2010 by Christian Brabandt</div><div class='line' id='LC14'>"			   The VIM LICENSE applies to histwin.vim </div><div class='line' id='LC15'>"			   (see |copyright|) except use "NrrwRgn.vim" </div><div class='line' id='LC16'>"			   instead of "Vim".</div><div class='line' id='LC17'>"			   No warranty, express or implied.</div><div class='line' id='LC18'>"	 *** ***   Use At-Your-Own-Risk!   *** ***</div><div class='line' id='LC19'>" GetLatestVimScripts: 3075 9 :AutoInstall: NrrwRgn.vim</div><div class='line' id='LC20'>"</div><div class='line' id='LC21'>" Init: {{{1</div><div class='line' id='LC22'>let s:cpo= &amp;cpo</div><div class='line' id='LC23'>if exists("g:loaded_nrrw_rgn") || &amp;cp</div><div class='line' id='LC24'>&nbsp;&nbsp;finish</div><div class='line' id='LC25'>endif</div><div class='line' id='LC26'>set cpo&amp;vim</div><div class='line' id='LC27'>let g:loaded_nrrw_rgn = 1</div><div class='line' id='LC28'><br/></div><div class='line' id='LC29'>" ------------------------------------------------------------------------------</div><div class='line' id='LC30'>" Public Interface: {{{1</div><div class='line' id='LC31'><br/></div><div class='line' id='LC32'>" Define the Command:</div><div class='line' id='LC33'>com! -range NarrowRegion :exe ":" . &lt;line1&gt; . ',' . &lt;line2&gt; . "call nrrwrgn#NrrwRgn()"</div><div class='line' id='LC34'>com! -range NR	 :exe ":" . &lt;line1&gt; . ',' . &lt;line2&gt; . "call nrrwrgn#NrrwRgn()"</div><div class='line' id='LC35'>com! -range NRV  :call nrrwrgn#VisualNrrwRgn(visualmode())</div><div class='line' id='LC36'>com! NW	 :exe ":" . line('w0') . ',' . line('w$') . "call nrrwrgn#NrrwRgn()"</div><div class='line' id='LC37'>com! NarrowWindow :exe ":" . line('w0') . ',' . line('w$') . "call nrrwrgn#NrrwRgn()"</div><div class='line' id='LC38'><br/></div><div class='line' id='LC39'>" Define the Mapping:</div><div class='line' id='LC40'>if !hasmapto('&lt;Plug&gt;NrrwrgnDo')</div><div class='line' id='LC41'>	xmap &lt;unique&gt; &lt;Leader&gt;nr &lt;Plug&gt;NrrwrgnDo</div><div class='line' id='LC42'>endif</div><div class='line' id='LC43'>xnoremap &lt;unique&gt; &lt;script&gt; &lt;Plug&gt;NrrwrgnDo &lt;sid&gt;VisualNrrwRgn</div><div class='line' id='LC44'>xnoremap &lt;sid&gt;VisualNrrwRgn :&lt;c-u&gt;call nrrwrgn#VisualNrrwRgn(visualmode())&lt;cr&gt;</div><div class='line' id='LC45'><br/></div><div class='line' id='LC46'>" Restore: "{{{1</div><div class='line' id='LC47'>let &amp;cpo=s:cpo</div><div class='line' id='LC48'>unlet s:cpo</div><div class='line' id='LC49'>" vim: ts=4 sts=4 fdm=marker com+=l\:\"</div><div class='line' id='LC50'>autoload/nrrwrgn.vim	[[[1</div><div class='line' id='LC51'>277</div><div class='line' id='LC52'>" NrrwRgn.vim - Narrow Region plugin for Vim</div><div class='line' id='LC53'>" -------------------------------------------------------------</div><div class='line' id='LC54'>" Version:	   0.9</div><div class='line' id='LC55'>" Maintainer:  Christian Brabandt &lt;cb@256bit.org&gt;</div><div class='line' id='LC56'>" Last Change: Thu, 20 May 2010 08:08:50 +0200</div><div class='line' id='LC57'>"</div><div class='line' id='LC58'>" Script: http://www.vim.org/scripts/script.php?script_id=3075 </div><div class='line' id='LC59'>" Copyright:   (c) 2009, 2010 by Christian Brabandt</div><div class='line' id='LC60'>"			   The VIM LICENSE applies to NrrwRgn.vim </div><div class='line' id='LC61'>"			   (see |copyright|) except use "NrrwRgn.vim" </div><div class='line' id='LC62'>"			   instead of "Vim".</div><div class='line' id='LC63'>"			   No warranty, express or implied.</div><div class='line' id='LC64'>"	 *** ***   Use At-Your-Own-Risk!   *** ***</div><div class='line' id='LC65'>" GetLatestVimScripts: 3075 9 :AutoInstall: NrrwRgn.vim</div><div class='line' id='LC66'>"</div><div class='line' id='LC67'>" Functions:</div><div class='line' id='LC68'><br/></div><div class='line' id='LC69'>fun! &lt;sid&gt;WarningMsg(msg)"{{{1</div><div class='line' id='LC70'>	echohl WarningMsg</div><div class='line' id='LC71'>	let msg = "NarrowRegion: " . a:msg</div><div class='line' id='LC72'>	if exists(":unsilent") == 2</div><div class='line' id='LC73'>		unsilent echomsg msg</div><div class='line' id='LC74'>	else</div><div class='line' id='LC75'>		echomsg msg</div><div class='line' id='LC76'>	endif</div><div class='line' id='LC77'>	echohl Normal</div><div class='line' id='LC78'>	let v:errmsg = msg</div><div class='line' id='LC79'>endfun "}}}</div><div class='line' id='LC80'>fun! &lt;sid&gt;Init()"{{{1</div><div class='line' id='LC81'>&nbsp;&nbsp;&nbsp;&nbsp;if !exists("s:instn")</div><div class='line' id='LC82'>		let s:instn=1</div><div class='line' id='LC83'>&nbsp;&nbsp;&nbsp;&nbsp;else</div><div class='line' id='LC84'>		let s:instn+=1</div><div class='line' id='LC85'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC86'>	if !exists("s:nrrw_rgn_lines")</div><div class='line' id='LC87'>		let s:nrrw_rgn_lines = {}</div><div class='line' id='LC88'>	endif</div><div class='line' id='LC89'>	let s:nrrw_rgn_lines[s:instn] = {}</div><div class='line' id='LC90'>&nbsp;&nbsp;&nbsp;&nbsp;let s:nrrw_winname='Narrow_Region'</div><div class='line' id='LC91'><br/></div><div class='line' id='LC92'>&nbsp;&nbsp;&nbsp;&nbsp;" Customization</div><div class='line' id='LC93'>&nbsp;&nbsp;&nbsp;&nbsp;let s:nrrw_rgn_vert = (exists("g:nrrw_rgn_vert")  ? g:nrrw_rgn_vert   : 0)</div><div class='line' id='LC94'>&nbsp;&nbsp;&nbsp;&nbsp;let s:nrrw_rgn_wdth = (exists("g:nrrw_rgn_wdth")  ? g:nrrw_rgn_wdth   : 20)</div><div class='line' id='LC95'>&nbsp;&nbsp;&nbsp;&nbsp;let s:nrrw_rgn_hl   = (exists("g:nrrw_rgn_hl")    ? g:nrrw_rgn_hl     : "WildMenu")</div><div class='line' id='LC96'>&nbsp;&nbsp;&nbsp;&nbsp;let s:nrrw_rgn_nohl = (exists("g:nrrw_rgn_nohl")  ? g:nrrw_rgn_nohl   : 0)</div><div class='line' id='LC97'><br/></div><div class='line' id='LC98'>&nbsp;&nbsp;&nbsp;&nbsp;let s:debug=1</div><div class='line' id='LC99'>	if exists("s:debug") &amp;&amp; s:debug</div><div class='line' id='LC100'>		com! NI :call &lt;sid&gt;WarningMsg("Instance: ".s:instn)</div><div class='line' id='LC101'>		com! NJ :call &lt;sid&gt;WarningMsg("Data: ".string(s:nrrw_rgn_lines))</div><div class='line' id='LC102'>	endif</div><div class='line' id='LC103'><br/></div><div class='line' id='LC104'>endfun </div><div class='line' id='LC105'><br/></div><div class='line' id='LC106'>fun! &lt;sid&gt;NrwRgnWin() "{{{1</div><div class='line' id='LC107'>	let s:nrrw_winname .= '_' . s:instn</div><div class='line' id='LC108'>&nbsp;&nbsp;&nbsp;&nbsp;let nrrw_win = bufwinnr('^'.s:nrrw_winname.'$')</div><div class='line' id='LC109'>&nbsp;&nbsp;&nbsp;&nbsp;if nrrw_win != -1</div><div class='line' id='LC110'>		exe ":noa " . nrrw_win . 'wincmd w'</div><div class='line' id='LC111'>		silent %d _</div><div class='line' id='LC112'>		noa wincmd p</div><div class='line' id='LC113'>&nbsp;&nbsp;&nbsp;&nbsp;else</div><div class='line' id='LC114'>		exe s:nrrw_rgn_wdth . (s:nrrw_rgn_vert?'v':'') . "sp " . s:nrrw_winname</div><div class='line' id='LC115'>		setl noswapfile buftype=acwrite bufhidden=wipe foldcolumn=0 nobuflisted winfixwidth winfixheight</div><div class='line' id='LC116'>		let nrrw_win = bufwinnr("")</div><div class='line' id='LC117'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC118'>&nbsp;&nbsp;&nbsp;&nbsp;return nrrw_win</div><div class='line' id='LC119'>endfu</div><div class='line' id='LC120'><br/></div><div class='line' id='LC121'>fun! nrrwrgn#NrrwRgn() range  "{{{1</div><div class='line' id='LC122'>	let o_lz = &amp;lz</div><div class='line' id='LC123'>	let s:o_s  = @/</div><div class='line' id='LC124'>	set lz</div><div class='line' id='LC125'>	let orig_buf=bufnr('')</div><div class='line' id='LC126'><br/></div><div class='line' id='LC127'>	" initialize Variables</div><div class='line' id='LC128'>	call &lt;sid&gt;Init()</div><div class='line' id='LC129'>	" Protect the original buffer,</div><div class='line' id='LC130'>	" so you won't accidentally modify those lines,</div><div class='line' id='LC131'>	" that might later be overwritten</div><div class='line' id='LC132'>	setl noma</div><div class='line' id='LC133'>	let ft=&amp;l:ft</div><div class='line' id='LC134'>	let s:nrrw_rgn_lines[s:instn].startline = [ a:firstline, 0 ]</div><div class='line' id='LC135'>	let s:nrrw_rgn_lines[s:instn].endline   = [ a:lastline, 0 ]</div><div class='line' id='LC136'>	"let s:nrrw_rgn_lines[s:instn].startline = [ a:firstline, 0 ]</div><div class='line' id='LC137'>	"let s:nrrw_rgn_lines[s:instn].endline   = [ a:lastline, 0 ]</div><div class='line' id='LC138'>	if exists("s:nrrw_rgn_lines[s:instn].matchid")</div><div class='line' id='LC139'>	    " if you call :NarrowRegion several times, without widening </div><div class='line' id='LC140'>	    " the previous region, b:matchid might already be defined so</div><div class='line' id='LC141'>	    " make sure, the previous highlighting is removed.</div><div class='line' id='LC142'>	    call matchdelete(s:nrrw_rgn_lines[s:instn].matchid)</div><div class='line' id='LC143'>	endif</div><div class='line' id='LC144'>	if !s:nrrw_rgn_nohl</div><div class='line' id='LC145'>	    let s:nrrw_rgn_lines[s:instn].matchid =  matchadd(s:nrrw_rgn_hl, </div><div class='line' id='LC146'>		\&lt;sid&gt;GeneratePattern(</div><div class='line' id='LC147'>		\s:nrrw_rgn_lines[s:instn].startline, </div><div class='line' id='LC148'>		\s:nrrw_rgn_lines[s:instn].endline, </div><div class='line' id='LC149'>		\'V')) "set the highlighting</div><div class='line' id='LC150'>	endif</div><div class='line' id='LC151'>	let a=getline(</div><div class='line' id='LC152'>	    \s:nrrw_rgn_lines[s:instn].startline[0], </div><div class='line' id='LC153'>	    \s:nrrw_rgn_lines[s:instn].endline[0])</div><div class='line' id='LC154'>	let win=&lt;sid&gt;NrwRgnWin()</div><div class='line' id='LC155'>	exe ':noa ' win 'wincmd w'</div><div class='line' id='LC156'>	let b:orig_buf = orig_buf</div><div class='line' id='LC157'>	call setline(1, a)</div><div class='line' id='LC158'>	setl nomod</div><div class='line' id='LC159'>	com! -buffer WidenRegion :call nrrwrgn#WidenRegion(0) |sil bd!|call &lt;sid&gt;NrrwRgnAuCmd(0)</div><div class='line' id='LC160'>	call &lt;sid&gt;NrrwRgnAuCmd(1)</div><div class='line' id='LC161'><br/></div><div class='line' id='LC162'>	" restore settings</div><div class='line' id='LC163'>	let &amp;l:ft = ft</div><div class='line' id='LC164'>	let &amp;lz   = o_lz</div><div class='line' id='LC165'>endfun</div><div class='line' id='LC166'><br/></div><div class='line' id='LC167'>fu! s:WriteNrrwRgn(...) "{{{1</div><div class='line' id='LC168'>&nbsp;&nbsp;&nbsp;&nbsp;if &amp;l:mod &amp;&amp; exists("a:1") &amp;&amp; a:1</div><div class='line' id='LC169'>		" Write the buffer back to the original buffer</div><div class='line' id='LC170'>		setl nomod</div><div class='line' id='LC171'>		exe ":WidenRegion"</div><div class='line' id='LC172'>&nbsp;&nbsp;&nbsp;&nbsp;else</div><div class='line' id='LC173'>		" Close the Narrowed Window</div><div class='line' id='LC174'>		call setbufvar(b:orig_buf, '&amp;ma', 1)</div><div class='line' id='LC175'>		"close!</div><div class='line' id='LC176'>		exe ':noa' . bufwinnr(b:orig_buf) . 'wincmd w'</div><div class='line' id='LC177'>		if exists("s:nrrw_rgn_lines[s:instn].matchid")</div><div class='line' id='LC178'>			call matchdelete(s:nrrw_rgn_lines[s:instn].matchid)</div><div class='line' id='LC179'>			unlet s:nrrw_rgn_lines[s:instn].matchid</div><div class='line' id='LC180'>		endif</div><div class='line' id='LC181'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC182'>endfun</div><div class='line' id='LC183'><br/></div><div class='line' id='LC184'>fu! nrrwrgn#WidenRegion(vmode) "{{{1</div><div class='line' id='LC185'>&nbsp;&nbsp;&nbsp;&nbsp;let nrw_buf  = bufnr('')</div><div class='line' id='LC186'>&nbsp;&nbsp;&nbsp;&nbsp;let orig_win = bufwinnr(b:orig_buf)</div><div class='line' id='LC187'>&nbsp;&nbsp;&nbsp;&nbsp;let cont     = getline(1,'$')</div><div class='line' id='LC188'>&nbsp;&nbsp;&nbsp;&nbsp;call &lt;sid&gt;SaveRestoreRegister(1)</div><div class='line' id='LC189'>&nbsp;&nbsp;&nbsp;&nbsp;exe ':noa' . orig_win . 'wincmd w'</div><div class='line' id='LC190'>&nbsp;&nbsp;&nbsp;&nbsp;if !(&amp;l:ma)</div><div class='line' id='LC191'>		setl ma</div><div class='line' id='LC192'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC193'>&nbsp;&nbsp;&nbsp;&nbsp;if a:vmode "charwise, linewise or blockwise selection </div><div class='line' id='LC194'>		call setreg('a', join(cont, "\n") . "\n", s:nrrw_rgn_lines[s:instn].vmode)</div><div class='line' id='LC195'>		if s:nrrw_rgn_lines[s:instn].vmode == 'v'</div><div class='line' id='LC196'>		   " in characterwise selection, remove trailing \n</div><div class='line' id='LC197'>		   call setreg('a', substitute(@a, '\n$', '', ''), </div><div class='line' id='LC198'>			   \s:nrrw_rgn_lines[s:instn].vmode)</div><div class='line' id='LC199'>		endif</div><div class='line' id='LC200'>		exe "keepj" s:nrrw_rgn_lines[s:instn].startline[0]</div><div class='line' id='LC201'>		exe "keepj norm!" s:nrrw_rgn_lines[s:instn].startline[1] . '|'</div><div class='line' id='LC202'>		exe "keepj norm!" s:nrrw_rgn_lines[s:instn].vmode</div><div class='line' id='LC203'>		exe "keepj" s:nrrw_rgn_lines[s:instn].endline[0]</div><div class='line' id='LC204'>		exe "keepj norm!" s:nrrw_rgn_lines[s:instn].endline[1] . '|'</div><div class='line' id='LC205'>		norm! "aP</div><div class='line' id='LC206'>		let [ s:nrrw_rgn_lines[s:instn].startline, </div><div class='line' id='LC207'>			 \s:nrrw_rgn_lines[s:instn].endline ] = &lt;sid&gt;RetVisRegionPos()</div><div class='line' id='LC208'>&nbsp;&nbsp;&nbsp;&nbsp;else "linewise selection because we started the NarrowRegion with the command NarrowRegion(0)</div><div class='line' id='LC209'>		if s:nrrw_rgn_lines[s:instn].endline[0]==line('$')</div><div class='line' id='LC210'>			let delete_last_line=1</div><div class='line' id='LC211'>		else</div><div class='line' id='LC212'>			let delete_last_line=0</div><div class='line' id='LC213'>		endif</div><div class='line' id='LC214'>		exe ':silent :'.s:nrrw_rgn_lines[s:instn].startline[0].','</div><div class='line' id='LC215'>			\.s:nrrw_rgn_lines[s:instn].endline[0].'d _'</div><div class='line' id='LC216'>		call append((s:nrrw_rgn_lines[s:instn].startline[0]-1),cont)</div><div class='line' id='LC217'>	    let  s:nrrw_rgn_lines[s:instn].endline[0] =</div><div class='line' id='LC218'>			\s:nrrw_rgn_lines[s:instn].startline[0] + len(cont) -1</div><div class='line' id='LC219'>	    if delete_last_line</div><div class='line' id='LC220'>			:$d _</div><div class='line' id='LC221'>	    endif</div><div class='line' id='LC222'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC223'>&nbsp;&nbsp;&nbsp;&nbsp;call &lt;sid&gt;SaveRestoreRegister(0)</div><div class='line' id='LC224'>&nbsp;&nbsp;&nbsp;&nbsp;let  @/=s:o_s</div><div class='line' id='LC225'>&nbsp;&nbsp;&nbsp;&nbsp;" jump back to narrowed window</div><div class='line' id='LC226'>&nbsp;&nbsp;&nbsp;&nbsp;exe ':noa' . bufwinnr(nrw_buf) . 'wincmd w'</div><div class='line' id='LC227'>&nbsp;&nbsp;&nbsp;&nbsp;"call &lt;sid&gt;NrrwRgnAuCmd(0)</div><div class='line' id='LC228'>&nbsp;&nbsp;&nbsp;&nbsp;"exe ':silent :bd!' nrw_buf</div><div class='line' id='LC229'>endfu</div><div class='line' id='LC230'><br/></div><div class='line' id='LC231'>fu! &lt;sid&gt;SaveRestoreRegister(mode) "{{{1</div><div class='line' id='LC232'>&nbsp;&nbsp;&nbsp;&nbsp;if a:mode</div><div class='line' id='LC233'>	let s:savereg  = getreg('a')</div><div class='line' id='LC234'>	let s:saveregt = getregtype('a')</div><div class='line' id='LC235'>&nbsp;&nbsp;&nbsp;&nbsp;else</div><div class='line' id='LC236'>	call setreg('a', s:savereg, s:saveregt)</div><div class='line' id='LC237'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC238'>endfu!</div><div class='line' id='LC239'><br/></div><div class='line' id='LC240'>fu! nrrwrgn#VisualNrrwRgn(mode) "{{{1</div><div class='line' id='LC241'>&nbsp;&nbsp;&nbsp;&nbsp;if empty(a:mode)</div><div class='line' id='LC242'>		" in case, visual mode wasn't entered, visualmode()</div><div class='line' id='LC243'>		" returns an empty string and in that case, we finish</div><div class='line' id='LC244'>		" here</div><div class='line' id='LC245'>		call &lt;sid&gt;WarningMsg("There was no region visually selected!")</div><div class='line' id='LC246'>		return</div><div class='line' id='LC247'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC248'>&nbsp;&nbsp;&nbsp;&nbsp;" This beeps, when called from command mode</div><div class='line' id='LC249'>&nbsp;&nbsp;&nbsp;&nbsp;" e.g. by using :NRV, so using :sil!</div><div class='line' id='LC250'>&nbsp;&nbsp;&nbsp;&nbsp;" else exiting visual mode</div><div class='line' id='LC251'>&nbsp;&nbsp;&nbsp;&nbsp;exe "sil! norm! \&lt;ESC&gt;"</div><div class='line' id='LC252'>&nbsp;&nbsp;&nbsp;&nbsp;" stop visualmode</div><div class='line' id='LC253'>&nbsp;&nbsp;&nbsp;&nbsp;let o_lz = &amp;lz</div><div class='line' id='LC254'>&nbsp;&nbsp;&nbsp;&nbsp;let s:o_s  = @/</div><div class='line' id='LC255'>&nbsp;&nbsp;&nbsp;&nbsp;set lz</div><div class='line' id='LC256'>&nbsp;&nbsp;&nbsp;&nbsp;let s:nrrw_rgn_lines[s:instn].vmode=a:mode</div><div class='line' id='LC257'>&nbsp;&nbsp;&nbsp;&nbsp;" Protect the original buffer,</div><div class='line' id='LC258'>&nbsp;&nbsp;&nbsp;&nbsp;" so you won't accidentally modify those lines,</div><div class='line' id='LC259'>&nbsp;&nbsp;&nbsp;&nbsp;" that will later be overwritten</div><div class='line' id='LC260'>&nbsp;&nbsp;&nbsp;&nbsp;setl noma</div><div class='line' id='LC261'>&nbsp;&nbsp;&nbsp;&nbsp;let orig_buf=bufnr('')</div><div class='line' id='LC262'>&nbsp;&nbsp;&nbsp;&nbsp;call &lt;sid&gt;SaveRestoreRegister(1)</div><div class='line' id='LC263'><br/></div><div class='line' id='LC264'>&nbsp;&nbsp;&nbsp;&nbsp;call &lt;sid&gt;Init()</div><div class='line' id='LC265'>&nbsp;&nbsp;&nbsp;&nbsp;let ft=&amp;l:ft</div><div class='line' id='LC266'>&nbsp;&nbsp;&nbsp;&nbsp;let [ s:nrrw_rgn_lines[s:instn].startline, s:nrrw_rgn_lines[s:instn].endline ] = &lt;sid&gt;RetVisRegionPos()</div><div class='line' id='LC267'>&nbsp;&nbsp;&nbsp;&nbsp;if exists("s:nrrw_rgn_lines[s:instn].matchid")</div><div class='line' id='LC268'>		" if you call :NarrowRegion several times, without widening </div><div class='line' id='LC269'>		" the previous region, b:matchid might already be defined so</div><div class='line' id='LC270'>		" make sure, the previous highlighting is removed.</div><div class='line' id='LC271'>		call matchdelete(s:nrrw_rgn_lines[s:instn].matchid)</div><div class='line' id='LC272'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC273'>&nbsp;&nbsp;&nbsp;&nbsp;if !s:nrrw_rgn_nohl</div><div class='line' id='LC274'>		let s:nrrw_rgn_lines[s:instn].matchid =  matchadd(s:nrrw_rgn_hl, </div><div class='line' id='LC275'>		\&lt;sid&gt;GeneratePattern(s:nrrw_rgn_lines[s:instn].startline, s:nrrw_rgn_lines[s:instn].endline, s:nrrw_rgn_lines[s:instn].vmode))</div><div class='line' id='LC276'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC277'>&nbsp;&nbsp;&nbsp;&nbsp;norm gv"ay</div><div class='line' id='LC278'>&nbsp;&nbsp;&nbsp;&nbsp;let win=&lt;sid&gt;NrwRgnWin()</div><div class='line' id='LC279'>&nbsp;&nbsp;&nbsp;&nbsp;exe ':noa ' win 'wincmd w'</div><div class='line' id='LC280'>&nbsp;&nbsp;&nbsp;&nbsp;let b:orig_buf = orig_buf</div><div class='line' id='LC281'>&nbsp;&nbsp;&nbsp;&nbsp;silent put a</div><div class='line' id='LC282'>&nbsp;&nbsp;&nbsp;&nbsp;silent 0d _</div><div class='line' id='LC283'>&nbsp;&nbsp;&nbsp;&nbsp;setl nomod</div><div class='line' id='LC284'>&nbsp;&nbsp;&nbsp;&nbsp;com! -buffer WidenRegion :call nrrwrgn#WidenRegion(1)|sil bd!|call &lt;sid&gt;NrrwRgnAuCmd(0)</div><div class='line' id='LC285'>&nbsp;&nbsp;&nbsp;&nbsp;call &lt;sid&gt;NrrwRgnAuCmd(1)</div><div class='line' id='LC286'>&nbsp;&nbsp;&nbsp;&nbsp;call &lt;sid&gt;SaveRestoreRegister(0)</div><div class='line' id='LC287'><br/></div><div class='line' id='LC288'>&nbsp;&nbsp;&nbsp;&nbsp;" restore settings</div><div class='line' id='LC289'>&nbsp;&nbsp;&nbsp;&nbsp;let &amp;l:ft = ft</div><div class='line' id='LC290'>&nbsp;&nbsp;&nbsp;&nbsp;let &amp;lz   = o_lz</div><div class='line' id='LC291'>endfu</div><div class='line' id='LC292'><br/></div><div class='line' id='LC293'>fu! &lt;sid&gt;NrrwRgnAuCmd(enable) "{{{1</div><div class='line' id='LC294'>&nbsp;&nbsp;&nbsp;&nbsp;if a:enable</div><div class='line' id='LC295'>		exe "aug NrrwRgn" . s:instn</div><div class='line' id='LC296'>			au!</div><div class='line' id='LC297'>			au BufWriteCmd &lt;buffer&gt; nested :call s:WriteNrrwRgn(1)</div><div class='line' id='LC298'>			au BufWipeout,BufDelete &lt;buffer&gt; nested :call s:WriteNrrwRgn()</div><div class='line' id='LC299'>		aug end</div><div class='line' id='LC300'>&nbsp;&nbsp;&nbsp;&nbsp;else</div><div class='line' id='LC301'>		exe "aug NrrwRgn" .  s:instn</div><div class='line' id='LC302'>		au!</div><div class='line' id='LC303'>		aug end</div><div class='line' id='LC304'>		exe "aug! NrrwRgn" . s:instn</div><div class='line' id='LC305'>		if s:instn&gt;0</div><div class='line' id='LC306'>			unlet s:nrrw_rgn_lines[s:instn]</div><div class='line' id='LC307'>			let s:instn-=1</div><div class='line' id='LC308'>		endif</div><div class='line' id='LC309'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC310'>endfun</div><div class='line' id='LC311'><br/></div><div class='line' id='LC312'>fu! &lt;sid&gt;RetVisRegionPos() "{{{1</div><div class='line' id='LC313'>&nbsp;&nbsp;&nbsp;&nbsp;let startline = [ getpos("'&lt;")[1], virtcol("'&lt;") ]</div><div class='line' id='LC314'>&nbsp;&nbsp;&nbsp;&nbsp;let endline   = [ getpos("'&gt;")[1], virtcol("'&gt;") ]</div><div class='line' id='LC315'>&nbsp;&nbsp;&nbsp;&nbsp;return [ startline, endline ]</div><div class='line' id='LC316'>endfu</div><div class='line' id='LC317'><br/></div><div class='line' id='LC318'>fun! &lt;sid&gt;GeneratePattern(startl, endl, mode) "{{{1</div><div class='line' id='LC319'>&nbsp;&nbsp;&nbsp;&nbsp;if a:mode ==# ''</div><div class='line' id='LC320'>	return '\%&gt;' . (a:startl[0]-1) . 'l\&amp;\%&gt;' . (a:startl[1]-1) . 'v\&amp;\%&lt;' . (a:endl[0]+1) . 'l\&amp;\%&lt;' . (a:endl[1]+1) . 'v'</div><div class='line' id='LC321'>&nbsp;&nbsp;&nbsp;&nbsp;elseif a:mode ==# 'v'</div><div class='line' id='LC322'>	return '\%&gt;' . (a:startl[0]-1) . 'l\&amp;\%&gt;' . (a:startl[1]-1) . 'v\_.*\%&lt;' . (a:endl[0]+1) . 'l\&amp;\%&lt;' . (a:endl[1]+1) . 'v'</div><div class='line' id='LC323'>&nbsp;&nbsp;&nbsp;&nbsp;else</div><div class='line' id='LC324'>	return '\%&gt;' . (a:startl[0]-1) . 'l\&amp;\%&lt;' . (a:endl[0]+1) . 'l'</div><div class='line' id='LC325'>&nbsp;&nbsp;&nbsp;&nbsp;endif</div><div class='line' id='LC326'>endfun "}}}</div><div class='line' id='LC327'><br/></div><div class='line' id='LC328'>" vim: ts=4 sts=4 fdm=marker com+=l\:\" fdl=0</div><div class='line' id='LC329'>doc/NarrowRegion.txt	[[[1</div><div class='line' id='LC330'>211</div><div class='line' id='LC331'>*NrrwRgn.txt*   A Narrow Region Plugin (similar to Emacs)</div><div class='line' id='LC332'><br/></div><div class='line' id='LC333'>Author:  Christian Brabandt &lt;cb@256bit.org&gt;</div><div class='line' id='LC334'>Version: 0.9 Thu, 20 May 2010 08:08:50 +0200</div><div class='line' id='LC335'><br/></div><div class='line' id='LC336'>Copyright: (c) 2009, 2010 by Christian Brabandt         </div><div class='line' id='LC337'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The VIM LICENSE applies to NrrwRgnPlugin.vim and NrrwRgnPlugin.txt</div><div class='line' id='LC338'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(see |copyright|) except use NrrwRgnPlugin instead of "Vim".</div><div class='line' id='LC339'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;NO WARRANTY, EXPRESS OR IMPLIED.  USE AT-YOUR-OWN-RISK.</div><div class='line' id='LC340'><br/></div><div class='line' id='LC341'><br/></div><div class='line' id='LC342'>==============================================================================</div><div class='line' id='LC343'>1. Contents                                     *NarrowRegion*  *NrrwRgnPlugin*</div><div class='line' id='LC344'><br/></div><div class='line' id='LC345'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1.  Contents.....................................: |NrrwRgnPlugin|</div><div class='line' id='LC346'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;2.  NrrwRgn Manual...............................: |NrrwRgn-manual|</div><div class='line' id='LC347'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;2.1   NrrwRgn Configuration......................: |NrrwRgn-config|</div><div class='line' id='LC348'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;3.  NrrwRgn Feedback.............................: |NrrwRgn-feedback|</div><div class='line' id='LC349'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;4.  NrrwRgn History..............................: |NrrwRgn-history|</div><div class='line' id='LC350'><br/></div><div class='line' id='LC351'>==============================================================================</div><div class='line' id='LC352'>2. NrrwRgn Manual                                       *NrrwRgn-manual*</div><div class='line' id='LC353'><br/></div><div class='line' id='LC354'>Functionality</div><div class='line' id='LC355'><br/></div><div class='line' id='LC356'>This plugin is based on a discussion in comp.editors (see the thread at</div><div class='line' id='LC357'>http://groups.google.com/group/comp.editors/browse_frm/thread/0f562d97f80dde13)</div><div class='line' id='LC358'><br/></div><div class='line' id='LC359'>Narrowing means focussing on a region and making the rest inaccessible. You</div><div class='line' id='LC360'>simply select the region, call :NarrowRegion and the selected part will open</div><div class='line' id='LC361'>in a new scratch buffer. The rest of the file will be protected, so you won't</div><div class='line' id='LC362'>accidentally modify that buffer. In the new buffer, you can do a global</div><div class='line' id='LC363'>replace, search or anything else to modify that part. When you are finished,</div><div class='line' id='LC364'>simply write that buffer (e.g. by |:w|) and your modifications will be put in</div><div class='line' id='LC365'>the original buffer making it accessible again.</div><div class='line' id='LC366'><br/></div><div class='line' id='LC367'>NrrwRgn allows you to either select a line based selection using an Ex-command</div><div class='line' id='LC368'>or you can simply use any visual selected region and press your prefered key</div><div class='line' id='LC369'>combination to open that selection in a new buffer.</div><div class='line' id='LC370'><br/></div><div class='line' id='LC371'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*:NarrowRegion* *:NR*</div><div class='line' id='LC372'>:[range]NarrowRegion        When [range] is omited, select only the current</div><div class='line' id='LC373'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;line, else use the lines in the range given and </div><div class='line' id='LC374'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;open it in a new Scratch Window. </div><div class='line' id='LC375'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Whenever you are finished modifying that region</div><div class='line' id='LC376'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;simply write the buffer.</div><div class='line' id='LC377'><br/></div><div class='line' id='LC378'>:[range]NR                  This is a shortcut for :NarrowRegion</div><div class='line' id='LC379'><br/></div><div class='line' id='LC380'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*:NarrowWindow* *:NW*</div><div class='line' id='LC381'>:NarrowWindow               Select only the range that is visible the current</div><div class='line' id='LC382'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;window and open it in a new Scratch Window. </div><div class='line' id='LC383'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Whenever you are finished modifying that region</div><div class='line' id='LC384'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;simply write the buffer.</div><div class='line' id='LC385'><br/></div><div class='line' id='LC386'>:NW                         This is a shortcut for :NarrowWindow</div><div class='line' id='LC387'><br/></div><div class='line' id='LC388'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*:WidenRegion*</div><div class='line' id='LC389'>:WidenRegion                This command is only available in the narrowed </div><div class='line' id='LC390'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;scratch window. If the buffer has been modified,</div><div class='line' id='LC391'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;the contents will be put back on the original</div><div class='line' id='LC392'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;buffer. If it isn't modified, closes the scratch</div><div class='line' id='LC393'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;window.</div><div class='line' id='LC394'><br/></div><div class='line' id='LC395'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*:NRV*</div><div class='line' id='LC396'>:NRW                        Opened the narrowed window for the region that was</div><div class='line' id='LC397'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;last selected in visual mode</div><div class='line' id='LC398'><br/></div><div class='line' id='LC399'>You can also start visual mode and have the selected region being narrowed. In</div><div class='line' id='LC400'>this mode, NarrowRegion allows you to block select |CTRL-V| , character select</div><div class='line' id='LC401'>|v| or linewise select |V| a region. Then press &lt;Leader&gt;nr where &lt;Leader&gt; by</div><div class='line' id='LC402'>default is set to '\', unless you have set it to something different (see</div><div class='line' id='LC403'>|&lt;Leader&gt;| for information how to change this) and the selected range will</div><div class='line' id='LC404'>open in a new scratch buffer. This key combination only works in |Visual-mode|</div><div class='line' id='LC405'><br/></div><div class='line' id='LC406'>When finished, simply write that Narrowed Region window, from which you want</div><div class='line' id='LC407'>to take the modifications in your original file. </div><div class='line' id='LC408'><br/></div><div class='line' id='LC409'>It is possible, to recursively open a Narrowed Window on top of an already</div><div class='line' id='LC410'>narrowed window. This sounds a little bit silly, but this makes it possible,</div><div class='line' id='LC411'>to have several narrowed windows, which you can use for several different</div><div class='line' id='LC412'>things, e.g. If you have 2 different buffers opened and you want to diff a</div><div class='line' id='LC413'>certain region of each of those 2 buffers, simply open a Narrowed Window for</div><div class='line' id='LC414'>each buffer, and execute |:diffthis| in each narrowed window. </div><div class='line' id='LC415'><br/></div><div class='line' id='LC416'>You can then interactively merge those 2 windows. And when you are finished,</div><div class='line' id='LC417'>simply write the narrowed window and the changes will be taken back into the</div><div class='line' id='LC418'>original buffer.</div><div class='line' id='LC419'>==============================================================================</div><div class='line' id='LC420'>2.1 NrrwRgn Configuration                                    *NrrwRgn-config*</div><div class='line' id='LC421'><br/></div><div class='line' id='LC422'>NarrowRegion can be customized by setting some global variables. If you'd</div><div class='line' id='LC423'>like to open the narrowed windo as a vertical split buffer, simply set the</div><div class='line' id='LC424'>variable g:nrrw_rgn_vert to 1 in your |.vimrc| &gt;</div><div class='line' id='LC425'><br/></div><div class='line' id='LC426'>&nbsp;&nbsp;&nbsp;&nbsp;let g:nrrw_rgn_vert = 1</div><div class='line' id='LC427'>&lt;</div><div class='line' id='LC428'>------------------------------------------------------------------------------</div><div class='line' id='LC429'><br/></div><div class='line' id='LC430'>If you'd like to specify a certain width/height for you scratch buffer, then</div><div class='line' id='LC431'>set the variable g:nrrw_rgn_wdth in your |.vimrc| . This variable defines the</div><div class='line' id='LC432'>width or the nr of columns, if you have also set g:nrrw_rgn_vert. &gt;</div><div class='line' id='LC433'><br/></div><div class='line' id='LC434'>&nbsp;&nbsp;&nbsp;&nbsp;let g:nrrw_rgn_wdth = 30</div><div class='line' id='LC435'>&lt;</div><div class='line' id='LC436'>------------------------------------------------------------------------------</div><div class='line' id='LC437'><br/></div><div class='line' id='LC438'>By default, NarrowRegion highlights the region that has been selected</div><div class='line' id='LC439'>using the WildMenu highlighting (see |hl-WildMenu|). If you'd like to use a</div><div class='line' id='LC440'>different highlighting, set the variable g:nrrw_rgn_hl to your preferred</div><div class='line' id='LC441'>highlighting Group. For example to have the region highlighted like a search</div><div class='line' id='LC442'>result, you could put that in your |.vimrc| &gt;</div><div class='line' id='LC443'><br/></div><div class='line' id='LC444'>&nbsp;&nbsp;&nbsp;&nbsp;let g:nrrw_rgn_hl = 'Search'</div><div class='line' id='LC445'>&lt;</div><div class='line' id='LC446'>If you want to turn off the highlighting (because this can be disturbing, you</div><div class='line' id='LC447'>can set the global variable g:nrrw_rgn_nohl to 1 in your |.vimrc| &gt;</div><div class='line' id='LC448'><br/></div><div class='line' id='LC449'>&nbsp;&nbsp;&nbsp;&nbsp;let g:nrrw_rgn_nohl = 1</div><div class='line' id='LC450'>&lt;</div><div class='line' id='LC451'>------------------------------------------------------------------------------</div><div class='line' id='LC452'><br/></div><div class='line' id='LC453'>If you'd like to change the key combination, that starts the Narrowed Window</div><div class='line' id='LC454'>for you selected range, you could put this in your |.vimrc| &gt;</div><div class='line' id='LC455'><br/></div><div class='line' id='LC456'>&nbsp;&nbsp;&nbsp;xmap &lt;F3&gt; &lt;Plug&gt;NrrwrgnDo</div><div class='line' id='LC457'>&lt;</div><div class='line' id='LC458'>This will let &lt;F3&gt; open the Narrow-Window, but only if you have pressed it in</div><div class='line' id='LC459'>Visual Mode. It doesn't really make sense to map this combination to any other</div><div class='line' id='LC460'>mode, unless you want it to Narrow your last visually selected range.</div><div class='line' id='LC461'><br/></div><div class='line' id='LC462'>==============================================================================</div><div class='line' id='LC463'>3. NrrwRgn Feedback                                         *NrrwRgn-feedback*</div><div class='line' id='LC464'><br/></div><div class='line' id='LC465'>Feedback is always welcome. If you like the plugin, please rate it at the</div><div class='line' id='LC466'>vim-page:</div><div class='line' id='LC467'>http://www.vim.org/scripts/script.php?script_id=3075</div><div class='line' id='LC468'><br/></div><div class='line' id='LC469'>You can also follow the development of the plugin at github:</div><div class='line' id='LC470'>http://github.com/chrisbra/NrrwRgn</div><div class='line' id='LC471'><br/></div><div class='line' id='LC472'>Please don't hesitate to report any bugs to the maintainer, mentioned in the</div><div class='line' id='LC473'>third line of this document.</div><div class='line' id='LC474'><br/></div><div class='line' id='LC475'>==============================================================================</div><div class='line' id='LC476'>4. NrrwRgn History                                          *NrrwRgn-history*</div><div class='line' id='LC477'><br/></div><div class='line' id='LC478'>0.9: May 20, 2010</div><div class='line' id='LC479'><br/></div><div class='line' id='LC480'>- It is now possible to Narrow a window recursively. This allows to have</div><div class='line' id='LC481'>&nbsp;&nbsp;several narrowed windows, and allows for example to only diff certain</div><div class='line' id='LC482'>&nbsp;&nbsp;regions (as was suggested in a recent thread at the vim_use mailinglist:</div><div class='line' id='LC483'>&nbsp;&nbsp;http://groups.google.com/group/vim_use/msg/05d7fd9bd1556f0e) therefore, the</div><div class='line' id='LC484'>&nbsp;&nbsp;use for the g:nrrw_rgn_sepwin variable isn't necessary anymore.</div><div class='line' id='LC485'>- Small documentation updates</div><div class='line' id='LC486'><br/></div><div class='line' id='LC487'>0.8: May 18, 2010</div><div class='line' id='LC488'><br/></div><div class='line' id='LC489'>- the g:nrrw_rgn_sepwin variable can be used to force seperate Narrowed</div><div class='line' id='LC490'>&nbsp;&nbsp;Windows, so you could easily diff those windows.</div><div class='line' id='LC491'>- make the separating of several windows a little bit safer (look at the</div><div class='line' id='LC492'>&nbsp;&nbsp;bufnr(), so it should work without problems for several buffers)</div><div class='line' id='LC493'>- switch from script local variables to buffer local variables, so narrowing</div><div class='line' id='LC494'>&nbsp;&nbsp;for several buffers should work.</div><div class='line' id='LC495'>- set 'winfixheight' for narrowed window </div><div class='line' id='LC496'>- Added command :NRV (suggested by Charles Campbell, thanks!)</div><div class='line' id='LC497'>- added error handling, in case :NRV is called, without a selected region</div><div class='line' id='LC498'>- take care of beeps, when calling :NRV</div><div class='line' id='LC499'>- output WarningMsg</div><div class='line' id='LC500'><br/></div><div class='line' id='LC501'>0.7: May 17, 2010</div><div class='line' id='LC502'><br/></div><div class='line' id='LC503'>- really use the black hole register for deleting the old buffer contents in</div><div class='line' id='LC504'>&nbsp;&nbsp;the narrowed buffer (suggestion by esquifit in</div><div class='line' id='LC505'>&nbsp;&nbsp;http://groups.google.com/group/comp.editors/msg/3eb3e3a7c68597db)</div><div class='line' id='LC506'>- make autocommand nesting, so the highlighting will be removed when writing</div><div class='line' id='LC507'>&nbsp;&nbsp;the buffer contents.</div><div class='line' id='LC508'>- Use g:nrrw_rgn_nohl variable to disable highlighting (as this can be</div><div class='line' id='LC509'>&nbsp;&nbsp;disturbing).</div><div class='line' id='LC510'><br/></div><div class='line' id='LC511'>0.6: May 04, 2010</div><div class='line' id='LC512'><br/></div><div class='line' id='LC513'>- the previous version had problems restoring the orig buffer, this version</div><div class='line' id='LC514'>&nbsp;&nbsp;fixes it (highlighting and setl ma did not work correctly)</div><div class='line' id='LC515'><br/></div><div class='line' id='LC516'>0.5: May 04, 2010       </div><div class='line' id='LC517'><br/></div><div class='line' id='LC518'>- The mapping that allows for narrowing a visually selected range, did not</div><div class='line' id='LC519'>&nbsp;&nbsp;work.  (Fixed!)</div><div class='line' id='LC520'>- Make :WidenRegion work as expected (close the widened window) (unreleased)</div><div class='line' id='LC521'><br/></div><div class='line' id='LC522'>0.4: Apr 28, 2010       </div><div class='line' id='LC523'><br/></div><div class='line' id='LC524'>- Highlight narrowed region in the original buffer</div><div class='line' id='LC525'>- Save and Restore search-register</div><div class='line' id='LC526'>- Provide shortcut commands |:NR| </div><div class='line' id='LC527'>- Provide command |:NW| and |:NarrowWindow|</div><div class='line' id='LC528'>- Make plugin autoloadable</div><div class='line' id='LC529'>- Enable GLVS (see |:GLVS|)</div><div class='line' id='LC530'>- Provide Documenation (:h NarrowRegion)</div><div class='line' id='LC531'>- Distribute Plugin as vimball |pi_vimball.txt|</div><div class='line' id='LC532'><br/></div><div class='line' id='LC533'>0.3: Apr 28, 2010       </div><div class='line' id='LC534'><br/></div><div class='line' id='LC535'>- Initial upload</div><div class='line' id='LC536'>- development versions are available at the github repository</div><div class='line' id='LC537'>- put plugin on a public repository (http://github.com/chrisbra/NrrwRgn)</div><div class='line' id='LC538'><br/></div><div class='line' id='LC539'>==============================================================================</div><div class='line' id='LC540'>Modeline:</div><div class='line' id='LC541'>vim:tw=78:ts=8:ft=help:et</div><div class='line' id='LC542'><br/></div></pre></div>
            
          </td>
        </tr>
      </table>
    
  </div>


      </div>
    </div>

  


    </div>
  
      

      <div class="push"></div>
    </div>

    <div id="footer">
      <div class="site">
        <div class="info">
          <div class="links">
            <a href="http://github.com/blog"><b>Blog</b></a> |
            <a href="http://support.github.com">Support</a> |
            <a href="http://github.com/training">Training</a> |
            <a href="http://github.com/contact">Contact</a> |
            <a href="http://develop.github.com">API</a> |
            <a href="http://status.github.com">Status</a> |
            <a href="http://twitter.com/github">Twitter</a> |
            <a href="http://help.github.com">Help</a> |
            <a href="http://github.com/security">Security</a>
          </div>
          <div class="company">
            &copy;
            2010
            <span id="_rrt" title="0.11361s from fe2.rs.github.com">GitHub</span> Inc.
            All rights reserved. |
            <a href="/site/terms">Terms of Service</a> |
            <a href="/site/privacy">Privacy Policy</a>
          </div>
        </div>
        <div class="sponsor">
          <div>
            Powered by the <a href="http://www.rackspace.com ">Dedicated
            Servers</a> and<br/> <a href="http://www.rackspacecloud.com">Cloud
            Computing</a> of Rackspace Hosting<span>&reg;</span>
          </div>
          <a href="http://www.rackspace.com">
            <img alt="Dedicated Server" src="http://assets1.github.com/images/modules/footer/rackspace_logo.png?6a7a6020e25b692257a3621f6090d7adc7f3d74b" />
          </a>
        </div>
      </div>
    </div>

    
    
      <!-- current locale:  -->
      <div class="locales">
        <div class="site">
          <ul class="choices">
            
              
                <li><span class="current">English</span></li>
              
            
              
                
                  <li><a rel="nofollow" href="?locale=ca">Català</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=cs">Čeština</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=de">Deutsch</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=es">Español</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=fr">Français</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=hr">Hrvatski</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=id">Indonesia</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=it">Italiano</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=ja">日本語</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=nl">Nederlands</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=no">Norsk</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=pl">Polski</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=pt-BR">Português (BR)</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=sr">Српски</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=sv">Svenska</a></li>
                
              
            
              
                
                  <li><a rel="nofollow" href="?locale=zh">中文</a></li>
                
              
            
          </ul>
        </div>
      </div>
    

    <script>window._auth_token = "8a14604e1c1c0a0c49c5626cccd3b66ab584160b"</script>
    

    <script type="text/javascript">
      _kmq.push(['trackClick', 'entice_banner_link', 'Entice banner clicked']);
      
    </script>
    
  </body>
</html>

