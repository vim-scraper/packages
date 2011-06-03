" Vim plugin -- Vi-style editing for the cmdline
" General: {{{1
" File:		conomode.vim
" Created:	2008 Sep 28
" Last Change:	2010 Aug 20
" Rev Days:	21
" Author:	Andy Wokula <anwoku@yahoo.de>
" Version:	0.4 (macro, undo) (after 0.3.1)
" Credits:
"   inspired from a vim_use thread on vi-style editing in the bash (there
"   enabled with 'set -o vi').
"   Subject:  command line
"   Date:     25-09-2008

" CAUTION:	This script may crash Vim now and then!  (buggy
"		getcmdline()?) -- almost fixed since Vim7.3f BETA

" Description: {{{1
"   Implements a kind of Normal mode ( "Cmdline-Normal mode" ) for the
"   Command line.  Great fun if   :h cmdline-window   gets boring ;-)

" Usage: {{{1
" - when in Cmdline-mode, press <F4> to enter the new "Cmdline-Normal mode";
"   mode indicator: a colon ":" at the cursor, hiding the char under it
" - quit to Cmdline-mode with <Esc>, ":", or any unmapped key (which then
"   executes or inserts itself), or wait 60 seconds.

" Features So Far: {{{1
" - Motions: h l w b e W B E 0 ^ $ f{char} F{char} ; ,
"   also in Operator pending mode
" - Operators: d y c
"   these write to the unnamed register; c prompts for input()
" - Insert: I i a A
"   these commands prompt for input()
" - Shortcuts: yy D x X s C
"   yy -> 0y$, D -> d$, x -> dl, X -> dh, s -> cl, C -> c$
" - Simple Changes: r{char} ~
" - Putting: P p
"   puts the unnamed register
" - Repeating: .
"   repeatable commands: d r c ~ I i a A
" - Macros: q @
"   q starts[/stops] recording, @ executes, no register involved
" - Mode Switching:
"   <Esc> o O - back to Cmdline, <CR> - execute Cmdline
" - Undo: u U
"   redo with "U" (to keep c_CTRL-R working); undo information survives mode
"   switching; undo is unlimited
" - Count: can be given for most commands
" - Misc: <C-L> - redraw the Cmdline

" Incompatibilities: (some ...) {{{1
" - redo with "U" (instead of "<C-R>")
" - "q" and "@" don't ask for a register, "@" while recording a macro
"   immediately executes

" Small Differences:
" - there are only exclusive motions, e.g. "f{char}" does not include the
"   target char; but "e" works ok (jumps after the word), as well as "$"
"   (EOL position is after the last char)
" - at EOL, "x", "dl", "dw" etc. do not go left to delete at least one
"   character
" - typing "dx" does "x", ignoring "d"; same for similar situations
" - "c", "r", "~": no undo step if old and new text are equal; "i": no undo
"   step if nothing inserted
" - "I" also removes initial white space and colons
" - "yy" yanks characterwise

" Notes: {{{1
" - strange: utf-8 characters are garbled by the mode indicator; press
"   Ctrl-L to redraw
" - how to find out which keys are mapped in the mode?
"	:ConomodemacroLocal cmap <SID>:
" - mapping <SID>:<BS> (<BS> = a key code expanding to several bytes)
"   doesn't work; probably this is related to a known Vim bug:
"	:h todo|/These two abbreviations don't give the same result:
" - manipulation of cmdline and cursor position uses getcmdline(),
"   c_CTRL-\_e, c_CTRL-R_=, getcmdpos(), setcmdpos()
" - ok: "3fx;;" -- do not remember the count for ";"
" - ok: "cw{text}<CR>5." -- "5." does "5cw{text}<CR>"

" TODO: {{{1
" - remove the [count] limits (e.g. don't expand "3h" to "<Left><Left><Left>")
" - M recording of ^R* (or remove ^R*)
" - M we need a beep: when executing, if one of the recorded commands fails,
"   the rest of the commands should not be executed
" - M beep: or just do  :normal <C-C>  plus  feedkeys( <SID>: ) ?
" - refactor s:count1?
" - recursive <F4>
" - (non-vi) "c", "i": if the last inserted char is a parenthesis (and it is
"   the only one), then "." will insert the corresponding paren
"
" + count: [1-9][0-9]* enable zero after one non-zero
" + count with multiplication: 2d3w = 6dw
" + count: f{char}, F{char}; r{char}; undo; put
" + "c" is repeatable
" + BF compare old and new text case sensitive
" + BF for now, disable recursive <F4>
" + BF opend(), allow "c" on zero characters
" + qcfx^UFoo^M@ works!! (with somes "x"es in the line)
" + BF qc$q recorded <SID>ocondollar<CR> instead of <SID>ocon$<CR>
" + doop_c: no default text, instead add old text to input history
" + BF doop_c: escape chars in input string for use in mapping (?) - yes!
" + implement "i", "I" and "A" with input(), like "c"
" + no need longer need to type <C-U> in "c{motion}<C-U>{text}<CR>"
" + BF undo/redo is now recorded
" + BF doop_c, opend: c{motion} should leave the cursor after the change
" + after playing a macro, undo the recorded commands at once.
"   ! KISS: let "@" remember the undo-index (mac_begin); when finished with
"   playing, remove the []s back to that index
" - search commands "/", "?", "n", "N": see conomode v0.3.2
" + command "a": move right first
" + continuous undo (don't break undo when switching to Cmdline-mode)
" + multi-byte support (!): some commands moved bytewise, not characterwise
"   (Mbyte); noch was übersehen?
" + BF <F4>-recursion prevention did <C-R>= within <C-\>e (not allowed)

" }}}

" Checks: {{{1
let s:cpo_sav = &cpo
set cpo&vim

if &cedit == "\<C-X>"
    echomsg "Conomode: Please do :set cedit& (only a warning)."
    " the user's new key for 'cedit' may come in the way
endif

" Some Local Variables: {{{1
let s:zaprev = {"f": "F", "F": "f"}
if !exists("s:undo")
    let s:undo = {}
endif
if !exists("s:quitnormal")
    let s:quitnormal = 1
endif
" Debug:
let g:conomode_dbg_undo = s:undo

" word forward patterns:
let s:wfpat = {
    \  "w": ['\k*\s*\zs', '\s*\zs', '\%(\k\@!\S\)*\s*\zs']
    \, "W": ['\S*\s*\zs', '\s*\zs', '\S*\s*\zs']
    \, "e": ['\k\+\zs', '\s*\%(\k\+\|\%(\k\@!\S\)*\)\zs', '\%(\k\@!\S\)*\zs']
    \, "E": ['\S\+\zs', '\s*\S*\zs', '\S*\zs']
    \}

let s:wbpat = {
    \  "b": ['\k*$', '\%(\k\+\|\%(\k\@!\S\)*\)\s*$', '\%(\k\@!\S\)*$']
    \, "B": ['\S*$', '\S*\s*$', '\S*$']
    \}

let s:cmdrev = {
    \  "caret": "^", "scolon": ";", "comma": ",", "dollar": "$"
    \, "put0-1": "P", "put1-1": "p" }

"}}}1

" Functions:
" Getpos: {{{1
func! s:forward_word(wm, count1)
    " wm - word motion: w, W or e
    let pat = s:wfpat[a:wm]
    let cnt = a:count1
    let gcp = getcmdpos()-1
    let cmdl = getcmdline()[gcp :]
    while 1
	let cpchar = matchstr(cmdl, '^.')
	if cpchar =~ '\k'
	    let matpos = match(cmdl, pat[0])
	elseif cpchar =~ '\s'
	    let matpos = match(cmdl, pat[1])
	else
	    let matpos = match(cmdl, pat[2])
	endif
	let cnt -= 1
	if cnt <= 0 || matpos <= 0
	    break
	endif
	let gcp += matpos
	let cmdl = cmdl[matpos :]
    endwhile
    let newcp = gcp + matpos
    return newcp + 1
endfunc

func! s:getpos_w()
    return s:forward_word("w", s:count1)
endfunc

func! s:getpos_W()
    return s:forward_word("W", s:count1)
endfunc

func! s:getpos_e()
    return s:forward_word("e", s:count1)
endfunc

func! s:getpos_E()
    return s:forward_word("E", s:count1)
endfunc

func! s:backward_word(wm, count1)
    let pat = s:wbpat[a:wm]
    let cnt = a:count1
    let gcp = getcmdpos()-1
    let cmdl = strpart(getcmdline(), 0, gcp)
    while gcp >= 1
	let cpchar = matchstr(cmdl, '.$')
	if cpchar =~ '\k'
	    let gcp = match(cmdl, pat[0])
	elseif cpchar =~ '\s'
	    let gcp = match(cmdl, pat[1])
	else
	    let gcp = match(cmdl, pat[2])
	endif
	let cnt -= 1
	if cnt <= 0 || gcp <= 0
	    break
	endif
	let cmdl = strpart(cmdl, 0, gcp)
    endwhile
    return gcp + 1
endfunc

func! s:getpos_b()
    return s:backward_word("b", s:count1)
endfunc

func! s:getpos_B()
    return s:backward_word("B", s:count1)
endfunc

func! s:getpos_h()
    " Omap mode only
    let gcp = getcmdpos()-1
    if s:count1 > gcp
	return 1
    elseif s:count1 == 1
	if gcp >= 8
	    return 1+gcp-8+match(strpart(getcmdline(), gcp-8, 8), '.$')
	else
	    return 1+match(strpart(getcmdline(), 0, gcp), '.$')
	endif
    endif
    let pos = match(strpart(getcmdline(), 0, gcp), '.\{'.s:count1.'}$')
    return pos >= 0 ? pos+1 : 1
endfunc

func! s:getpos_l()
    let gcp = getcmdpos()-1
    if s:count1 == 1
	return 1+matchend(getcmdline(), '.\|$', gcp)
    endif
    let cmdlsuf = strpart(getcmdline(), gcp)
    let lensuf = strlen(cmdlsuf)
    if s:count1 >= lensuf
	return 1+gcp+lensuf
    else
	return 1+gcp+matchend(cmdlsuf, '.\{'.s:count1.'}\|$')
    endif
endfunc

func! s:getpos_dollar()
    return strlen(getcmdline())+1
endfunc

func! s:getpos_0()
    return 1
endfunc

func! s:getpos_caret()
    return 1+match(getcmdline(), '\S')
endfunc

" Getzappos: {{{1
func! s:getzappos(zapcmd, ...)
    let cnt = s:count1
    if a:0 == 0
	if !s:from_mapping
	    call inputsave()
	    let aimchar = nr2char(getchar())
	    call inputrestore()
	else
	    let aimchar = nr2char(getchar())
	endif
	let s:lastzap = [a:zapcmd, aimchar]
	if s:recording
	    " call s:rec_chars(cnt, a:zapcmd.aimchar)
	    if s:zapmode == "n"
		let reczap = "<C-X>(eat)<SID>cono". a:zapcmd
	    else
		let reczap = "<C-X>(eat)<SID>ocon". a:zapcmd
	    endif
	    if s:zapmode == "o" && s:operator == "c"
		let s:rec_op_c = reczap."<CR>".aimchar
	    else
		call s:rec_chars(cnt, reczap."<CR>".aimchar."<SID>:")
	    endif
	endif
    else
	let aimchar = a:1
    endif
    let gcp = getcmdpos()-1
    let newcp = gcp
    if a:zapcmd ==# "f"
	let cmdl = getcmdline()[gcp+1 :]
	while 1
	    let matpos = stridx(cmdl, aimchar)
	    let cnt -= 1
	    if cnt <= 0 || matpos < 0
		break
	    endif
	    let newcp += matpos+1
	    let cmdl = cmdl[matpos+1 :]
	endwhile
	let newcp = matpos >= 0 ? newcp + matpos+1 : gcp
    else " F
	let cmdl = strpart(getcmdline(), 0, gcp)
	while 1
	    let newcp = strridx(cmdl, aimchar)
	    let cnt -= 1
	    if cnt <= 0 || newcp < 0
		break
	    endif
	    let cmdl = strpart(cmdl, 0, newcp)
	endwhile
	let newcp = newcp >= 0 ? newcp : gcp
    endif
    let s:beep = newcp == gcp
    return newcp + 1
endfunc

func! s:getpos_f()
    return s:getzappos("f")
endfunc

func! s:getpos_F()
    return s:getzappos("F")
endfunc

func! s:getpos_scolon()
    if exists("s:lastzap")
	return s:getzappos(s:lastzap[0], s:lastzap[1])
    else
	return getcmdpos()
    endif
endfunc

func! s:getpos_comma()
    if exists("s:lastzap")
	return s:getzappos(s:zaprev[s:lastzap[0]], s:lastzap[1])
    else
	return getcmdpos()
    endif
endfunc

" Move: {{{1
func! <sid>move(motion)
    let s:count1 = s:getcount1()
    call setcmdpos(s:getpos_{a:motion}())
    call s:rec_chars(s:count1, a:motion)
    return ""
endfunc

func! <sid>move_zap(zapcmd)
    let s:count1 = s:getcount1()
    let s:zapmode = "n"
    call setcmdpos(s:getzappos(a:zapcmd))
    return ""
endfunc

" Put: {{{1
func! <sid>edit_put(mode, reg, gcpoff, endoff)
    let coff = a:gcpoff
    if a:mode == 1
	" limit count to 500
	let cnt = min([s:getcount1(),500])
	let s:lastedit = ["edit_put", 0, a:reg, coff, a:endoff]
	let s:lastcount = cnt
	call s:rec_chars(cnt, "put". a:gcpoff. a:endoff)
    else
	let cnt = s:lastcount
    endif
    let gcp = getcmdpos()-1
    let cmdl = getcmdline()
    if coff == 1 && cmdl[gcp] == ""
	let coff = 0
    endif
    let boff = coff==0 ? 0 : matchend(strpart(cmdl, gcp, 8), '.')
    let ins = repeat(getreg(a:reg), cnt)
    if ins != ""
	" after undoing "p", move the cursor one left from the start of the
	" change
	call s:undo.add(0, "m", gcp, "")
	call s:undo.add(1, "i", gcp+boff, ins)
	call setcmdpos(gcp+1+strlen(ins)+boff+a:endoff)
    endif
    return strpart(cmdl, 0, gcp+boff). ins. strpart(cmdl, gcp+boff)
endfunc

" Edit: {{{1
func! <sid>edit_r(mode, ...)
    if a:mode == 1
	let cnt = s:getcount1()
	if !s:from_mapping
	    call inputsave()
	    let replchar = nr2char(getchar())
	    call inputrestore()
	else
	    let replchar = nr2char(getchar())
	endif
	let s:lastedit = ["edit_r", 0, replchar]
	let s:lastcount = cnt
	" we must have that damn replchar BEFORE the next <SID>:
	call s:rec_chars(cnt, "<C-X>(eat)<SID>conor<CR>".replchar."<SID>:")
    else
	let replchar = a:1
	let cnt = s:lastcount
    endif
    let gcp = getcmdpos()-1
    let cmdl = getcmdline()
    let ripos = matchend(cmdl, '.\{'.cnt.'}', gcp)
    if ripos >= 1
	let mid = cmdl[gcp : ripos-1]
	let newmid = repeat(replchar, cnt)
	if mid !=# newmid
	    call s:undo.add(0, "d", gcp, mid)
	    call s:undo.add(1, "i", gcp, newmid)
	endif
	return strpart(cmdl, 0, gcp). newmid. strpart(cmdl, ripos)
    else
	return cmdl
    endif
endfunc

func! <sid>edit_tilde(mode, ...)
    if a:mode == 1
	let cnt = s:getcount1()
	let s:lastedit = ["edit_tilde", 0]
	let s:lastcount = cnt
	call s:rec_chars(cnt, "~")
    else
	let cnt = s:lastcount
    endif
    let gcp = getcmdpos()-1
    let cmdl = getcmdline()
    let ripos = matchend(cmdl, '.\{1,'.cnt.'}', gcp)
    if ripos >= 1
	let mid = cmdl[gcp : ripos-1]
	" let newmid = substitute(mid, '\(\u\)\|\(\l\)', '\l\1\u\2', 'g')
	let newmid = substitute(mid, '\k', '\=toupper(submatch(0))==#submatch(0) ? tolower(submatch(0)) : toupper(submatch(0))', 'g')
	if mid !=# newmid
	    call s:undo.add(0, "d", gcp, mid)
	    call s:undo.add(1, "i", gcp, newmid)
	endif
	call setcmdpos(gcp+1 + strlen(newmid))
	return strpart(cmdl, 0, gcp). newmid. strpart(cmdl, ripos)
    else
	return cmdl
    endif
endfunc

func! <sid>setop(op)
    let s:operator = a:op
    let s:beep = 0
    call s:rec_chars("", a:op)
    return ""
endfunc

func! s:doop_d(str, pos, rep)
    let @@ = a:str
    call s:undo.add(1, "d", a:pos, a:str)
    call setcmdpos(a:pos + 1)
    return ""
endfunc

func! s:doop_y(str, pos, ...)
    let @@ = a:str
    call setcmdpos(a:pos + 1)
    return a:str
endfunc

" Insert: {{{1
func! s:doop_c(str, pos, rep)
    if s:beep && !s:from_mapping
	return a:str
    endif
    let @@ = a:str
    if !a:rep
	if !s:from_mapping
	    call histadd("@", a:str)
	    call inputsave()
	    let newtext = input("Change into:")
	    call inputrestore()
	else
	    let newtext = input("", a:str)
	endif
	let s:lastitext = newtext
	if s:recording
	    call s:rec_chars(s:count1, s:rec_op_c."<C-U>".s:MapEscape(newtext)."<CR><SID>:")
	endif
    else
	let newtext = s:lastitext
    endif
    if s:beep
	return a:str
    endif
    if a:str !=# newtext
	call s:undo.add(0, "d", a:pos, a:str)
	call s:undo.add(1, "i", a:pos, newtext)
    endif
    call setcmdpos(a:pos+1 + strlen(newtext))
    return newtext 
endfunc

func! <sid>insert(mode, cmd)
    if a:mode == 1
	let cnt = s:getcount1()
	let s:lastedit = ["insert", 0, a:cmd]
	let s:lastcount = cnt
	if !s:from_mapping
	    call inputsave()
	    let newtext = input("Insert:")
	    call inputrestore()
	else
	    let newtext = input("")
	endif
	let s:lastitext = newtext
	if s:recording
	    call s:rec_chars(cnt, a:cmd. "<C-X>(eat)". s:MapEscape(newtext). "<CR><SID>:")
	    " faced a crash without <C-X>(eat) (and mapesc)
	endif
    else
	let cnt = s:lastcount
	let newtext = s:lastitext
    endif
    let cmdl = getcmdline()
    if newtext != "" || a:cmd ==# "I"
	if a:cmd ==# "I"
	    let iwhite = matchstr(cmdl, '^[ \t:]*')
	    if iwhite == "" && newtext == ""
		return cmdl
	    endif
	    let gcp = 0
	    call s:undo.add(0, "d", gcp, iwhite)
	    let cmdl = strpart(cmdl, strlen(iwhite))
	elseif a:cmd ==# "a"
	    let gcp = matchend(cmdl, '^.\=', getcmdpos()-1)
	elseif a:cmd ==# "A"
	    let gcp = strlen(cmdl)
	else
	    let gcp = getcmdpos()-1
	endif
	let resulttext = repeat(newtext, cnt)
	call s:undo.add(1, "i", gcp, resulttext) 
	call setcmdpos(gcp+1 + strlen(resulttext))
	return strpart(cmdl, 0, gcp). resulttext. strpart(cmdl, gcp)
    else
	return cmdl
    endif
endfunc

let s:esctbl = {"|": "<Bar>", "<": "<lt>", "\r": '<C-V><CR>', "\n": '<C-V><NL>', "\e": '<C-V><Esc>'}
let s:escpat = '[|<[:cntrl:]]'

func! s:MapEscape(str)
    if a:str =~ s:escpat
	let str = substitute(a:str, s:escpat, '\=get(s:esctbl, submatch(0), " O.o ")', 'g')
	return str
    else
	return a:str
    endif
endfunc

" Opend: {{{1
func! <sid>opend(motion, ...)
    let motion = a:motion

    if a:0 == 0
	let s:count1 = s:getcount1()
	let s:lastedit = ["opend", motion, 0]
	let s:lastcount = s:count1
	let isrep = 0
	if s:recording
	    if s:operator == "c"
		" just without trailing "<SID>:"
		let mot = get(s:cmdrev, a:motion, a:motion)
		let s:rec_op_c = "<C-X>(eat)<SID>ocon".mot."<CR>"
	    else
		call s:rec_chars(s:count1, a:motion)
	    endif
	endif
    elseif a:1 == 1
	" zap motion
	let s:count1 = s:getcount1()
	let s:lastedit = ["opend", a:2, 0]
	let s:lastcount = s:count1
	let s:zapmode = "o"
	let isrep = 0
    else " e.g. a:1 == 0
	let s:count1 = s:lastcount
	let isrep = 1
    endif

    let gcp = getcmdpos()-1

    " cw,cW -> ce,cE (not on white space)
    if s:operator == "c" && motion ==? "w"
	\ && getcmdline()[gcp] =~ '\S'
	let motion = tr(motion, "wW", "eE")
    endif

    let tarpos = s:getpos_{motion}()-1

    " only exclusive "motions"
    let cmdl = getcmdline()
    if gcp < tarpos
	let [pos1, pos2] = [gcp, tarpos]
    elseif tarpos < gcp
	let [pos1, pos2] = [tarpos, gcp]
    "elseif gcp > 0 && (a:0 == 0 || a:1 != 0) && gcp==strlen(cmdl)
    "	 let [pos1, pos2] = [gcp-1, gcp]
    "	 let newcp = gcp - 1
    elseif s:operator == "c"
	" op c must accept everything to always eat ^U and ^M from rec
	let [pos1, pos2] = [gcp, gcp]
    else
	return cmdl
    endif

    let cmdlpart = strpart(cmdl, pos1, pos2-pos1)
    let newpart = s:doop_{s:operator}(cmdlpart, pos1, isrep)

    return strpart(cmdl,0,pos1). newpart. cmdl[pos2 :]
endfunc

" Repeat: {{{1
func! <sid>edit_dot()
    let cnt = s:getcount()
    call s:rec_chars(cnt, ".")
    if exists("s:lastedit")
	if cnt > 0
	    let s:lastcount = cnt
	endif
	return call("s:".s:lastedit[0], s:lastedit[1:])
    else
	return getcmdline()
    endif
endfunc

func! <sid>macro_rec()
    let s:counta = ""
    let s:countb = ""
    cmap <SID>:0 <SID>zero
    if !s:recording
	let s:recbuf = ""
	let s:recording = 1
	" call s:undo.mac_begin()
	call s:warn("START recording")
    else
	" call s:undo.mac_end()
	let s:recording = 0
	let g:CONOMODE_RECBUF = s:recbuf
	call s:warn("STOP recording")
    endif
    return ""
endfunc

" execute macro: duplicate macro count times, size limit=1000
func! <sid>macro_exec()
    if s:recording
	call s:undo.mac_end()
	let s:recording = 0
	let g:CONOMODE_RECBUF = s:recbuf
    endif
    let cnt = s:getcount1()
    if s:recbuf != ""
	let reclen = strlen(s:recbuf)
	if reclen * cnt > 1000
	    let cnt = max([1000 / reclen, 1])
	endif
	exec "cnoremap <script> <SID>macro_keys <SID>:".repeat(s:recbuf, cnt)
    else
	cnoremap <script> <SID>macro_keys <SID>:
    endif
    call s:undo.mac_begin()
    let s:from_mapping = 1
    return ""
endfunc

" special keys must be keycodes; a:1 - mode char : or ;
" sometimes we extra-check s:recording before calling this func, sometimes
" not
func! s:rec_chars(count1, str)
    if s:recording
	let str = get(s:cmdrev, a:str, a:str)
	let s:recbuf .= (a:count1>1 ? a:count1 : ""). str
    endif
endfunc

func! <sid>mapoff()
    call s:undo.mac_end()
    let s:from_mapping = 0
    return ""
endfunc

" Count: {{{1
func! s:getcount()
    let iszero = s:counta == "" && s:countb == ""
    let count1 = s:getcount1()
    return iszero ? 0 : count1
endfunc

func! s:getcount1()
    if s:counta != ""
	let cnta = s:counta + 0
	let s:counta = ""
	cmap <SID>:0 <SID>zero
    else
	let cnta = 1
    endif
    if s:countb != ""
	let cntb = s:countb + 0
	let s:countb = ""
	cnoremap <script> <SID>;0 <SID>ocon0<CR><SID>:
    else
	let cntb = 1
    endif
    return cnta * cntb
endfunc

func! <sid>counta(digit)
    if s:counta == ""
	cnoremap <script> <SID>:0 <SID>cono0<CR><SID>:
    endif
    let s:counta .= a:digit
    return ""
endfunc

func! <sid>countb(digit)
    if s:countb == ""
	cnoremap <script> <SID>;0 <SID>ocnt0<CR><SID>;
    endif
    let s:countb .= a:digit
    return ""
endfunc

func! <sid>eatcount(key)
    let s:counta = ""
    let s:countb = ""
    if a:key != "0"
	cmap <SID>:0 <SID>zero
    endif
    if s:recording
	call s:rec_chars(1, a:key)
    endif
    return ""
endfunc

" duplicate a basic motion count times (limit=500); call with empty
" argument to just eat the count
func! <sid>rep(key, reckey, ...)
    let cnt = min([s:getcount1()*(a:0 >= 1 ? a:1 : 1), 500])
    if s:recording
	call s:rec_chars("", repeat(a:reckey, cnt))
    endif
    return repeat(a:key, cnt)
endfunc

" Init: (more local variables) {{{1
func! <sid>set_tm()
    if s:quitnormal
	let s:tm_sav = &tm
	set timeoutlen=60000
    endif
    let s:quitnormal = 0
    let s:counta = ""
    let s:countb = ""
    call s:try_continue_undo()
    let s:recording = 0
    let s:recbuf = exists("g:CONOMODE_RECBUF") ? g:CONOMODE_RECBUF : ""

    " started conomode from a mapping? - commands with user input (r c f)
    " don't work, they will always query the user; but we can handle
    " 'internal' 'mappings':
    let s:from_mapping = 0
    " or check getchar(1) ?

    cmap <SID>:0 <SID>zero
    cnoremap <script> <SID>;0 <SID>ocon0<CR><SID>:
    return ""
endfunc

func! <sid>rst_tm(ms)
    let &tm = s:tm_sav
    let s:quitnormal = 1
    if a:ms > 0
	"DEBUG:
	call s:warn("Quit Conomode", a:ms)
    endif
    " let s:undo.list = [[]]
    " let s:undo.idx = 0
    let s:lastcmdline = getcmdline()
    return ""
endfunc

func! s:try_continue_undo()
    if !exists("s:lastcmdline")
	call s:undo.init()
	return
    endif

    let cmdl = getcmdline()
    if cmdl ==# s:lastcmdline
	return
    endif

    let lastcmdl = s:lastcmdline
    let patL = matchlist(lastcmdl, '^\(.\)\(.*\)$')[1:2]

    if empty(patL)
	let isfinal = cmdl == ""
	if lastcmdl != ""
	    call s:undo.add(isfinal, "d", 0, lastcmdl)
	endif
	if !isfinal
	    call s:undo.add(1, "i", 0, cmdl)
	endif
	return
    endif

    call map(patL, 'escape(v:val, ''\.*$^~['')')
    let forw_pat = '^\C'. patL[0]. (patL[1]=="" ? "" : '\%['. patL[1]. ']')

    let com_prefix = matchstr(cmdl, forw_pat)
    let lenpre = strlen(com_prefix)
    let cmdlrest = strpart(cmdl, lenpre)

    let lastcmdlrest = strpart(lastcmdl, lenpre)
    if lastcmdlrest =~ '.'
	let revlastcmdlrest = join(reverse(split(lastcmdlrest, '\m')),'')
	let patL = matchlist(revlastcmdlrest, '^\(.\)\(.*\)$')[1:2]
	call map(patL, 'escape(v:val, ''\.*$^~['')')
	let back_pat = '^\C'. patL[0]. (patL[1]=="" ? "" : '\%['. patL[1]. ']')

	let com_suffix = matchstr(join(reverse(split(cmdlrest, '\m')),''), back_pat)
	let lensuf = strlen(com_suffix)
    else
	let com_suffix = ""
	let lensuf = 0
    endif

    let deleted = strpart(lastcmdl, lenpre, strlen(lastcmdl)-lensuf-lenpre)
    let inserted = strpart(cmdl, lenpre, strlen(cmdl)-lensuf-lenpre)

    let isfinal = inserted == ""
    if deleted != ""
	call s:undo.add(isfinal, "d", lenpre, deleted) 
    endif
    if !isfinal
	call s:undo.add(1, "i", lenpre, inserted)
    endif
endfunc

" Undo: "{{{1
func! <sid>undo()
    if s:recording
	call s:rec_chars(s:getcount1(), "u")
    endif
    return s:undo.do()
endfunc

func! <sid>redo()
    if s:recording
	call s:rec_chars(s:getcount1(), "U")
    endif
    return s:undo.redo()
endfunc

func! <sid>clru()
    call s:undo.init()
    return ""
endfunc

func! s:undo.init()
    let self.list = [[]]
    let self.idx = 0
endfunc

func! s:undo.add(islast, dori, pos, str)
    let self.idx += 1
    call insert(self.list, [a:dori, a:pos, a:str], self.idx)
    if a:islast
	call self.stopseq()
    endif
endfunc

func! s:undo.stopseq()
    let self.idx += 1
    call insert(self.list, [], self.idx)
    if exists("self.list[self.idx+1]")
	call remove(self.list, self.idx+1, -1)
    endif
endfunc

func! s:undo.mac_begin()
    let self.mac_idx = self.idx
endfunc

func! s:undo.mac_end()
    if exists("self.mac_idx")
	let idx = self.idx - 1
	while idx > self.mac_idx
	    if empty(self.list[idx])
		call remove(self.list, idx)
		let self.idx -= 1
	    endif
	    let idx -= 1
	endwhile
	unlet self.mac_idx
    endif
endfunc

func! s:undo.do()
    " do undo, go backwards in the list
    let cmdl = getcmdline()
    let cnt = s:getcount1()
    while cnt >= 1 && self.idx >= 1
	let self.idx -= 1
	let item = get(self.list, self.idx, [])
	while !empty(item)
	    let [type, pos, str] = item
	    if type == "d"
		let left = strpart(cmdl, 0, pos)
		let right = strpart(cmdl, pos)
		let cmdl = left. str. right
	    elseif type == "i"
		let left = strpart(cmdl, 0, pos)
		let right = strpart(cmdl, pos + strlen(str))
		let cmdl = left. right
	    endif
	    call setcmdpos(pos+1)
	    let self.idx -= 1
	    let item = get(self.list, self.idx, [])
	endwhile
	let cnt -= 1
    endwhile
    return cmdl
endfunc

func! s:undo.redo()
    let cmdl = getcmdline()
    let cnt = s:getcount1()
    while cnt >= 1 && exists("self.list[self.idx+1]")
	let self.idx += 1
	let item = get(self.list, self.idx, [])
	while !empty(item)
	    let [type, pos, str] = item
	    if type == "d"
		let left = strpart(cmdl, 0, pos)
		let right = strpart(cmdl, pos + strlen(str))
		let cmdl = left. right
	    elseif type == "i"
		let left = strpart(cmdl, 0, pos)
		let right = strpart(cmdl, pos)
		let cmdl = left. str. right
	    endif
	    call setcmdpos(pos+1)
	    let self.idx += 1
	    let item = get(self.list, self.idx, [])
	endwhile
	let cnt -= 1
    endwhile
    return cmdl
endfunc

" Misc: {{{1
func! s:warn(...)
    echohl WarningMsg
    if a:0 == 0
	redraw
	echon matchstr(v:exception, ':\zs.*')
	sleep 1
    else
	echon a:1
	exec "sleep" (a:0>=2 ? a:2 : 300)."m"
    endif
    echohl None
endfunc

" func! <sid>exec(cmd)
"     try|exec a:cmd|catch|call s:warn()|endtry
"     return ""
" endfunc

"}}}1

" Mappings:
" Entering: Cmdline-Normal mode {{{1
if !hasmapto("<Plug>Conomode", "c")
    cmap <F4> <Plug>Conomode
endif

cmap <expr>	    <Plug>Conomode  getcmdtype()=="@" ? "<lt>F4>" : "<SID>Conomode"
cnoremap <script>   <SID>Conomode   <SID>set_tm<CR><SID>:
cnoremap <silent>   <SID>set_tm	    <C-R>=<sid>set_tm()

" Simple Movement: h l (0) $ {{{1
cnoremap <script>   <SID>zero	  <SID>prezero<CR><C-B><SID>:
cnoremap <silent>   <SID>prezero  <C-R>=<sid>eatcount("0")
cnoremap <script>   <SID>:$	  <SID>predoll<CR><C-E><SID>:
cnoremap <silent>   <SID>predoll  <C-R>=<sid>eatcount("$")

cnoremap <expr><script> <SID>:h <sid>rep("<Left>","h")."<SID>:"
cnoremap <expr><script> <SID>:l <sid>rep("<Right>","l")."<SID>:"
cnoremap <expr><script> <SID>:k <sid>rep("<Left>","k",&co)."<SID>:"
cnoremap <expr><script> <SID>:j <sid>rep("<Right>","j",&co)."<SID>:"

" Motions: ^ f{char} F{char} ; , w b e W B E {{{1
cnoremap <script>   <SID>:^	<SID>cono^<CR><SID>:
cnoremap <silent>   <SID>cono^	<C-R>=<sid>move("caret")

cnoremap <script>   <SID>:f	<SID>conof<CR><SID>:
cnoremap <silent>   <SID>conof	<C-R>=<sid>move_zap("f")
cnoremap <script>   <SID>:F	<SID>conoF<CR><SID>:
cnoremap <silent>   <SID>conoF	<C-R>=<sid>move_zap("F")
cnoremap <script>   <SID>:;	<SID>cono;<CR><SID>:
cnoremap <silent>   <SID>cono;	<C-R>=<sid>move("scolon")
cnoremap <script>   <SID>:,	<SID>cono,<CR><SID>:
cnoremap <silent>   <SID>cono,	<C-R>=<sid>move("comma")

cnoremap <script>   <SID>:w	<SID>conow<CR><SID>:
cnoremap <silent>   <SID>conow	<C-R>=<sid>move("w")
cnoremap <script>   <SID>:W	<SID>conoW<CR><SID>:
cnoremap <silent>   <SID>conoW	<C-R>=<sid>move("W")
cnoremap <script>   <SID>:b	<SID>conob<CR><SID>:
cnoremap <silent>   <SID>conob	<C-R>=<sid>move("b")
cnoremap <script>   <SID>:B	<SID>conoB<CR><SID>:
cnoremap <silent>   <SID>conoB	<C-R>=<sid>move("B")
cnoremap <script>   <SID>:e	<SID>conoe<CR><SID>:
cnoremap <silent>   <SID>conoe	<C-R>=<sid>move("e")
cnoremap <script>   <SID>:E	<SID>conoE<CR><SID>:
cnoremap <silent>   <SID>conoE	<C-R>=<sid>move("E")

"" History: k j {{{1
"cnoremap <script>   <SID>:k	<SID>clru<Up><SID>:
"cnoremap <script>   <SID>:j	<SID>clru<Down><SID>:
"cnoremap <expr>	    <SID>clru	<sid>clru()

" Shortcuts: D x X yy s C {{{1
cnoremap <script>   <SID>:D	<SID>:d$
cnoremap <script>   <SID>:x	<SID>:dl
cnoremap <script>   <SID>:X	<SID>:dh
cnoremap <script>   <SID>:yy	<SID>:0y$
cnoremap <script>   <SID>:s	<SID>:cl
cnoremap <script>   <SID>:C	<SID>:c$
cnoremap <script>   <SID>:Y	<SID>:y$
cnoremap <script>   <SID>:dd	<SID>:0d$

" Put: P p {{{1
cnoremap <script>   <SID>:P	<SID>conoP<CR><SID>:
cnoremap <silent>   <SID>conoP	<C-\>e<sid>edit_put(1,'"',0,-1)
cnoremap <script>   <SID>:p	<SID>conop<CR><SID>:
cnoremap <silent>   <SID>conop	<C-\>e<sid>edit_put(1,'"',1,-1)

" Operators: d y c {{{1
cnoremap <script>   <SID>:d	<SID>conod<CR><SID>;
cnoremap <silent>   <SID>conod	<C-R>=<sid>setop("d")
cnoremap <script>   <SID>:y	<SID>conoy<CR><SID>;
cnoremap <silent>   <SID>conoy	<C-R>=<sid>setop("y")

cnoremap <script>   <SID>:c	<SID>conoc<CR><SID>;
cnoremap <silent>   <SID>conoc	<C-R>=<sid>setop("c")

" Simple Changes: r ~ {{{1
cnoremap <script>   <SID>:r	<SID>conor<CR><SID>:
cnoremap <silent>   <SID>conor	<C-\>e<sid>edit_r(1)
cnoremap <script>   <SID>:~	<SID>cono~<CR><SID>:
cnoremap <silent>   <SID>cono~	<C-\>e<sid>edit_tilde(1)

" Insert: I i a A {{{1
cnoremap <script>   <SID>:I	<SID>conoI<CR><SID>:
cnoremap <silent>   <SID>conoI	<C-\>e<sid>insert(1,"I")
cnoremap <script>   <SID>:i	<SID>conoi<CR><SID>:
cnoremap <silent>   <SID>conoi	<C-\>e<sid>insert(1,"i")
cnoremap <script>   <SID>:a	<SID>conoa<CR><SID>:
cnoremap <silent>   <SID>conoa	<C-\>e<sid>insert(1,"a")
cnoremap <script>   <SID>:A	<SID>conoA<CR><SID>:
cnoremap <silent>   <SID>conoA	<C-\>e<sid>insert(1,"A")

" Undo: u U {{{1
cnoremap <script>   <SID>:u	<SID>conou<CR><SID>:
cnoremap <silent>   <SID>conou	<C-\>e<sid>undo()
cnoremap <script>   <SID>:U	<SID>conoU<CR><SID>:
cnoremap <silent>   <SID>conoU	<C-\>e<sid>redo()

" Repeating: . q Q @ {{{1
cnoremap <script>   <SID>:.	<SID>cono.<CR><SID>:
cnoremap <silent>   <SID>cono.	<C-\>e<sid>edit_dot()
cnoremap <script>   <SID>:q	<SID>conoq<CR><SID>:
cnoremap <silent>   <SID>conoq	<C-R>=<sid>macro_rec()
cmap		    <SID>:@	<SID>cono@a<SID>macro_keys<C-X>(mapoff)<SID>cono@b
cnoremap <silent>   <SID>cono@a	<C-R>=<sid>macro_exec()<CR>
cnoremap <silent>   <SID>:<C-X>(mapoff)	<C-R>=<sid>mapoff()
cnoremap <script>   <SID>cono@b	<CR><SID>:
cnoremap	    <SID>:<C-X>(eat)  <Nop>
cnoremap	    <SID>;<C-X>(eat)  <Nop>
" XXX same bug as with '<SID>:<BS>': '<SID>:<C-X>(mapoff)' works, but
" '<SID>:<SID>mapoff' not; this workaround is dirty: <C-X>(eat) typed by the
" user bypasses cleanup

" Count: 1 2 3 4 5 6 7 8 9 (0) {{{1
cnoremap <silent>   <SID>cono0	<C-R>=<sid>counta("0")
cnoremap <script>   <SID>:1	<SID>cono1<CR><SID>:
cnoremap <silent>   <SID>cono1	<C-R>=<sid>counta("1")
cnoremap <script>   <SID>:2	<SID>cono2<CR><SID>:
cnoremap <silent>   <SID>cono2	<C-R>=<sid>counta("2")
cnoremap <script>   <SID>:3	<SID>cono3<CR><SID>:
cnoremap <silent>   <SID>cono3	<C-R>=<sid>counta("3")
cnoremap <script>   <SID>:4	<SID>cono4<CR><SID>:
cnoremap <silent>   <SID>cono4	<C-R>=<sid>counta("4")
cnoremap <script>   <SID>:5	<SID>cono5<CR><SID>:
cnoremap <silent>   <SID>cono5	<C-R>=<sid>counta("5")
cnoremap <script>   <SID>:6	<SID>cono6<CR><SID>:
cnoremap <silent>   <SID>cono6	<C-R>=<sid>counta("6")
cnoremap <script>   <SID>:7	<SID>cono7<CR><SID>:
cnoremap <silent>   <SID>cono7	<C-R>=<sid>counta("7")
cnoremap <script>   <SID>:8	<SID>cono8<CR><SID>:
cnoremap <silent>   <SID>cono8	<C-R>=<sid>counta("8")
cnoremap <script>   <SID>:9	<SID>cono9<CR><SID>:
cnoremap <silent>   <SID>cono9	<C-R>=<sid>counta("9")

" Omap Motions: h l w W b B e E $ ^ {{{1
cnoremap <script>   <SID>;h	<SID>oconh<CR><SID>:
cnoremap <silent>   <SID>oconh	<C-\>e<sid>opend("h")
cnoremap <script>   <SID>;l	<SID>oconl<CR><SID>:
cnoremap <silent>   <SID>oconl	<C-\>e<sid>opend("l")
cnoremap <script>   <SID>;w	<SID>oconw<CR><SID>:
cnoremap <silent>   <SID>oconw	<C-\>e<sid>opend("w")
cnoremap <script>   <SID>;W	<SID>oconW<CR><SID>:
cnoremap <silent>   <SID>oconW	<C-\>e<sid>opend("W")
cnoremap <script>   <SID>;b	<SID>oconb<CR><SID>:
cnoremap <silent>   <SID>oconb	<C-\>e<sid>opend("b")
cnoremap <script>   <SID>;B	<SID>oconB<CR><SID>:
cnoremap <silent>   <SID>oconB	<C-\>e<sid>opend("B")
cnoremap <script>   <SID>;e	<SID>ocone<CR><SID>:
cnoremap <silent>   <SID>ocone	<C-\>e<sid>opend("e")
cnoremap <script>   <SID>;E	<SID>oconE<CR><SID>:
cnoremap <silent>   <SID>oconE	<C-\>e<sid>opend("E")
cnoremap <script>   <SID>;$	<SID>ocon$<CR><SID>:
cnoremap <silent>   <SID>ocon$	<C-\>e<sid>opend("dollar")
cnoremap <silent>   <SID>ocon0	<C-\>e<sid>opend("0")
cnoremap <script>   <SID>;^	<SID>ocon^<CR><SID>:
cnoremap <silent>   <SID>ocon^	<C-\>e<sid>opend("caret")

" Omap count: 1 2 3 4 5 6 7 8 9 (0) {{{1
cnoremap <silent>   <SID>ocnt0	<C-R>=<sid>countb("0")
cnoremap <script>   <SID>;1	<SID>ocnt1<CR><SID>;
cnoremap <silent>   <SID>ocnt1	<C-R>=<sid>countb("1")
cnoremap <script>   <SID>;2	<SID>ocnt2<CR><SID>;
cnoremap <silent>   <SID>ocnt2	<C-R>=<sid>countb("2")
cnoremap <script>   <SID>;3	<SID>ocnt3<CR><SID>;
cnoremap <silent>   <SID>ocnt3	<C-R>=<sid>countb("3")
cnoremap <script>   <SID>;4	<SID>ocnt4<CR><SID>;
cnoremap <silent>   <SID>ocnt4	<C-R>=<sid>countb("4")
cnoremap <script>   <SID>;5	<SID>ocnt5<CR><SID>;
cnoremap <silent>   <SID>ocnt5	<C-R>=<sid>countb("5")
cnoremap <script>   <SID>;6	<SID>ocnt6<CR><SID>;
cnoremap <silent>   <SID>ocnt6	<C-R>=<sid>countb("6")
cnoremap <script>   <SID>;7	<SID>ocnt7<CR><SID>;
cnoremap <silent>   <SID>ocnt7	<C-R>=<sid>countb("7")
cnoremap <script>   <SID>;8	<SID>ocnt8<CR><SID>;
cnoremap <silent>   <SID>ocnt8	<C-R>=<sid>countb("8")
cnoremap <script>   <SID>;9	<SID>ocnt9<CR><SID>;
cnoremap <silent>   <SID>ocnt9	<C-R>=<sid>countb("9")

" Omap Zap Motions: f F ; , {{{1
cnoremap <script>   <SID>;f	<SID>oconf<CR><SID>:
cnoremap <silent>   <SID>oconf	<C-\>e<sid>opend("f",1,"scolon")
cnoremap <script>   <SID>;F	<SID>oconF<CR><SID>:
cnoremap <silent>   <SID>oconF	<C-\>e<sid>opend("F",1,"scolon")
cnoremap <script>   <SID>;;	<SID>ocon;<CR><SID>:
cnoremap <silent>   <SID>ocon;	<C-\>e<sid>opend("scolon")
cnoremap <script>   <SID>;,	<SID>ocon,<CR><SID>:
cnoremap <silent>   <SID>ocon,	<C-\>e<sid>opend("comma")

" Goodies: c_CTRL-R_*, ^L {{{1
" with undo, count, dot-repeat, XXX recording
cnoremap <script>   <SID>:<C-R>	<SID>"
cnoremap <script>   <SID>"*	<SID>CtlR*<CR><SID>:
cnoremap <silent>   <SID>CtlR*	<C-\>e<sid>edit_put(1,"*",0,0)
cmap		    <SID>"	<SID>rst_tm<SID><CR><C-R>

" cnorem <script>   <SID>:<C-L>	<C-R>=<sid>exec("redraw")<CR><SID>:
cnoremap <script>   <SID>:<C-L>	<Space><C-H><SID>:

" Mode Switching: {{{1
" From Cmdline-Normal mode 
" to Cmdline mode (start over)
cmap		    <SID>::	<SID>:dd<C-X>(eat)<SID>rst_tq<SID><CR>

" no timeout with these keys:
cmap		    <SID>:<Esc>	<SID>rst_tq<SID><CR>
cmap		    <SID>:o	<SID>:<Esc>
cmap		    <SID>:O	<SID>:<Esc>
" no map for "<SID>:<Esc>" makes <Esc> return to Normal mode immediately
cmap		    <SID>:<CR>	<SID>rst_tq<SID><CR><CR>

" to Cmdline mode (key not mapped -> make <SID>: do nothing)
cnoremap <script>   <SID>:	<SID>rst_tm<CR>
cnoremap <silent>   <SID>rst_tm <C-R>=<sid>rst_tm(200)
cnoremap <silent>   <SID>rst_tq <C-R>=<sid>rst_tm(0)
cnoremap	    <SID><CR>	<CR>

" Cmdline-Omap mode to Cmdline-Normal mode (implicit)
cmap		    <SID>;	<SID>:
" maybe:
cnoremap <script>   <SID>;<Esc> <SID>:

"}}}1

" Debug:
com! -nargs=* -complete=command ConomodemacroLocal <args>

" Modeline: {{{1
let &cpo = s:cpo_sav
" vim:set ts=8 sts=4 sw=4 fdm=marker:
