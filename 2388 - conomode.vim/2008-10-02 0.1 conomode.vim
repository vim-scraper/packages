" Vim plugin -- Vi-style editing for the cmdline
" General: {{{1
" File:		conomode.vim
" Created:	2008 Sep 28
" Last Change:	2008 Oct 02
" Rev Days:	5
" Author:	Andy Wokula <anwoku@yahoo.de>
" Version:	0.1
" Credits:
"   inspired from a vim_use thread on vi-style editing in the bash (there
"   enabled with 'set -o vi').
"   Subject:  command line
"   Date:     25-09-2008

" CAUTION:	This script may crash Vim now and then!  (buggy
"		getcmdline()?)

" Description: {{{1
"   Implements a kind of Normal mode ( "Cmdline-Normal mode" ) for the
"   Command line.  Great fun if   :h cmdline-window   gets boring ;-)

" Mode Switching: {{{1
" - press c_<F4> to enter the new "Cmdline-Normal mode"; mode indicator: a
"   colon ":" at the cursor, hiding the char under it
" - quit to Normal mode with <Esc>, as usual!
" - quit to Command-line mode with i a I A :, or any unmapped key (which
"   then executes or inserts itself), or wait 10 seconds.

" Features So Far: {{{1
" - Motions: h l w b e W B 0 ^ $ f{char} F{char} ; ,
"   also in Operator pending mode
" - More Motions: E
" - Operators: d y c
"   writing to the unnamed register; c prompts for input()
" - Shortcuts: yy D x X s C
"   yy -> 0y$, D -> d$, x -> dl, X -> dh, s -> cl, C -> c$
" - Simple Changes: r{char}
" - Putting: p
"   puts the unnamed register
" - Repeating: .
"   repeatable commands: d r c
" - History (Cmdline): k j
" - Mode Switching: i a I A : (c) ...
" - Undo: u U
"   redo with "U" (to keep c_CTRL-R working); undo information does not
"   survive mode switching
" - Count: for most commands

" Incompatibilities: (some ...) {{{1
" - "i{text}<Esc>" can be surprising -- "i" only switches to Cmdline mode
"   and <Esc> goes back to Normal mode!  Instead, do "i{text}<F4>"; but:
"   undo, count and dot-repetition is not available for this command.
" - type "c{motion}<C-U>{text}<CR>" instead of "c{motion}{text}<Esc>"

" Small Differences:
" - there are only exclusive motions, e.g. f{char} does not include the
"   target char; but "e" works ok (jumps after the word), as well as "$"
"   (EOL position is after the last char)
" - at EOL, "x", "dl", "dw" etc. do not go left to delete at least one
"   character
" - typing "dx" does "x", ignoring "d"; same for similar situations
" - "c", "r": no undo step if old and new text are equal

" Notes: {{{1
" - How to find out which keys are mapped in the mode?	Not easily at the
"   moment.  You can try  :scr  (to get the script id [N]) and then
"	:cmap <SNR>[N]_
" - mapping <SID>:<BS> (<BS> = a key code expanding to several bytes)
"   doesn't work; probably this is related to a known Vim bug:
"	:h todo|/These two abbreviations don't give the same result:
" - manipulation of cmdline and cursor position using getcmdline(),
"   c_CTRL-\_e, c_CTRL-R_=, getcmdpos(), setcmdpos()
" - ok: "3fx;;" -- do not remember the count for ";"
" - ok: "cw{text}<CR>5." -- "5." does "5cw{text}<CR>"

" TODO: {{{1
" - multi byte chars in the command-line?
" - omap mode for E
" - allow "c" on zero characters
" - remove s:count1
" - if Cono mode was left with [iaIA] and re-entered with <F4>, try to
"   analyze the (unknown) changes as an "i{text}<Esc>" command; continue the
"   undo history then
" ? /{pat} n N
" + count: [1-9][0-9]* enable zero after one non-zero
" + count with multiplication: 2d3w = 6dw
" + count: f{char}, F{char}; r{char}; undo; put
" + clear undo info when doing k j
" + "c" is repeatable

" }}}

" Checks: {{{1
let s:cpo_sav = &cpo
set cpo&vim

if &cedit != "\<C-F>"
    echomsg "Conomode: Please do :set cedit& (only a warning)."
endif

" Some Local Variables: {{{1
let s:quitnormal = 1
let s:zaprev = {"f": "F", "F": "f"}
let s:undo = {}
" Debug:
let g:dbg_undo = s:undo

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
	let cpchar = cmdl[0]
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
	let cpchar = cmdl[gcp-1]
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
    " omap mode only
    return max([getcmdpos()-s:count1, 1])
endfunc

func! s:getpos_l()
    return -max([-(strlen(getcmdline())+1), -(getcmdpos()+s:count1)])
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
	call inputsave()
	let aimchar = nr2char(getchar())
	call inputrestore()
	let s:lastzap = [a:zapcmd, aimchar]
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
    return ""
endfunc

func! <sid>move_zap(zapcmd)
    let s:count1 = s:getcount1()
    call setcmdpos(s:getzappos(a:zapcmd))
    return ""
endfunc

" Put: {{{1
func! <sid>edit_put(mode, reg, gcpoff, endoff)
    let off = a:gcpoff
    if a:mode == 1
	" limit count to 500
	let cnt = -max([-s:getcount1(),-500])
	let s:lastedit = ["edit_put", 0, a:reg, off, a:endoff]
	let s:lastcount = cnt
    else
	let cnt = s:lastcount
    endif
    let gcp = getcmdpos()-1
    let cmdl = getcmdline()
    if off == 1 && cmdl[gcp] == ""
	let off = 0
    endif
    let ins = repeat(getreg(a:reg), cnt)
    if ins != ""
	" after undoing "p", move the cursor one left from the start of the
	" change
	call s:undo.add(0, "m", gcp, "")
	call s:undo.add(1, "i", gcp+off, ins)
	call setcmdpos(gcp+1+strlen(ins)+off+a:endoff)
    endif
    return strpart(cmdl, 0, gcp+off). ins. strpart(cmdl, gcp+off)
endfunc

" Edit: {{{1
func! <sid>edit_r(mode, ...)
    if a:mode == 1
	let cnt = s:getcount1()
	call inputsave()
	let replchar = nr2char(getchar())
	call inputrestore()
	let s:lastedit = ["edit_r", 0, replchar]
	let s:lastcount = cnt
    else
	let replchar = a:1
	let cnt = s:lastcount
    endif
    let gcp = getcmdpos()-1
    let cmdl = getcmdline()
    let ripos = matchend(cmdl, '.\{'.cnt.'}', gcp)
    if ripos >= 0
	let mid = cmdl[gcp : ripos-1]
	let newmid = repeat(replchar, cnt)
	if mid != newmid
	    call s:undo.add(0, "d", gcp, mid)
	    call s:undo.add(1, "i", gcp, newmid)
	endif
	return strpart(cmdl, 0, gcp). newmid. strpart(cmdl, ripos)
    else
	return cmdl
    endif
endfunc

func! <sid>setop(op)
    let s:operator = a:op
    return ""
endfunc

func! s:doop_d(str, pos, rep)
    let @@ = a:str
    call s:undo.add(1, "d", a:pos, a:str)
    return ""
endfunc

func! s:doop_y(str, ...)
    let @@ = a:str
    return a:str
endfunc

func! s:doop_c(str, pos, rep)
    let @@ = a:str
    if !a:rep
	call inputsave()
	let newtext = input("Change into:", a:str)
	call inputrestore()
	let s:lastitext = newtext
    else
	let newtext = s:lastitext
    endif
    if a:str != newtext
	call s:undo.add(0, "d", a:pos, a:str)
	call s:undo.add(1, "i", a:pos, newtext)
    endif
    return newtext 
endfunc

" Opend: {{{1
func! <sid>opend(motion, ...)
    let motion = a:motion

    if a:0 == 0
	let s:count1 = s:getcount1()
	let s:lastedit = ["opend", motion, 0]
	let s:lastcount = s:count1
	let isrep = 0
    elseif a:1 == 1
	let s:count1 = s:getcount1()
	let s:lastedit = ["opend", a:2, 0]
	let s:lastcount = s:count1
	let isrep = 0
    else " e.g. a:1 == 0
	let s:count1 = s:lastcount
	let isrep = 1
    endif

    let gcp = getcmdpos()-1

    " cw,cW -> ce,cE (not on white space)
    if s:operator ==# "c" && motion ==? "w"
	\ && getcmdline()[gcp] =~ '\S'
	let motion = tr(motion, "wW", "eE")
    endif

    let tarpos = s:getpos_{motion}()-1

    " only exclusive "motions"
    let cmdl = getcmdline()
    if gcp < tarpos
	let [pos1, pos2] = [gcp, tarpos]
	let newcp = gcp
    elseif tarpos < gcp
	let [pos1, pos2] = [tarpos, gcp]
	let newcp = tarpos
    "elseif gcp > 0 && (a:0 == 0 || a:1 != 0) && gcp==strlen(cmdl)
    "	 let [pos1, pos2] = [gcp-1, gcp]
    "	 let newcp = gcp - 1
    else
	return cmdl
    endif

    let cmdlpart = cmdl[pos1 : pos2-1]
    let newpart = s:doop_{s:operator}(cmdlpart, pos1, isrep)
    call setcmdpos(newcp + 1)

    return strpart(cmdl,0,pos1). newpart. cmdl[pos2 :]
endfunc

" Repeat: {{{1
func! <sid>edit_dot()
    let cnt = s:getcount()
    if exists("s:lastedit")
	if cnt > 0
	    let s:lastcount = cnt
	endif
	return call("s:".s:lastedit[0], s:lastedit[1:])
    else
	return getcmdline()
    endif
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
	cnoremap <script> <SID>:0 <SID>eatcnt<C-B><SID>:
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

" duplicate a basic motion count times (limit=500); call with empty
" argument to just eat the count
func! <sid>rep(key)
    return repeat(a:key, -max([-s:getcount1(),-500]))
endfunc

" Init: (more local variables) {{{1
func! <sid>set_tm()
    if s:quitnormal
	let s:tm_sav = &tm
	set timeoutlen=10000
    endif
    let s:quitnormal = 0
    let s:counta = ""
    let s:countb = ""
    let s:undo.list = [[]]
    let s:undo.idx = 0
    cnoremap <script> <SID>:0 <SID>eatcnt<C-B><SID>:
    cnoremap <script> <SID>;0 <SID>ocon0<CR><SID>:
    return ""
endfunc

func! <sid>rst_tm()
    let &tm = s:tm_sav
    let s:quitnormal = 1
    let s:undo.list = []
    let s:undo.idx = 0
    return ""
endfunc

" Undo: "{{{1
func! <sid>undo()
    return s:undo.do()
endfunc

func! <sid>redo()
    return s:undo.redo()
endfunc

func! s:undo.add(islast, ...)
    let self.idx += 1
    " omitting copy() crashes Vim later
    call insert(self.list, copy(a:000), self.idx)
    if a:islast
	let self.idx += 1
	call insert(self.list, [], self.idx)
	if exists("self.list[self.idx+1]")
	    call remove(self.list, self.idx+1, -1)
	endif
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

func! <sid>clru()
    let s:undo.idx = 0
    let s:undo.list = [[]]
    return ""
endfunc
"}}}1

" Mappings:
" Entering: Cmdline-Normal mode {{{1
cnoremap <script>   <F4>	<SID>set_tm<CR><SID>:
cnoremap <silent>   <SID>set_tm <C-R>=<sid>set_tm()

" Simple Movement: h l (0) $ {{{1
cnoremap <expr>     <SID>eatcnt <sid>rep("")

cnoremap <expr><script> <SID>:h <sid>rep("<Left>")."<SID>:"
cnoremap <expr><script> <SID>:l <sid>rep("<Right>")."<SID>:"
cnoremap <script>   <SID>:$	<SID>eatcnt<C-E><SID>:

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

" History: k j {{{1
cnoremap <script>   <SID>:k	<SID>clru<Up><SID>:
cnoremap <script>   <SID>:j	<SID>clru<Down><SID>:
cnoremap <expr>	    <SID>clru	<sid>clru()

" Shortcuts: D x X yy s C {{{1
cnoremap <script>   <SID>:D	<SID>:d$
cnoremap <script>   <SID>:x	<SID>:dl
cnoremap <script>   <SID>:X	<SID>:dh
cnoremap <script>   <SID>:yy	<SID>:0y$
cnoremap <script>   <SID>:s	<SID>:cl
cnoremap <script>   <SID>:C	<SID>:c$
cnoremap <script>   <SID>:Y	<SID>:y$

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

" Simple Changes: r {{{1
cnoremap <script>   <SID>:r	<SID>conor<CR><SID>:
cnoremap <silent>   <SID>conor	<C-\>e<sid>edit_r(1)

" Undo: u U {{{1
cnoremap <script>   <SID>:u	<SID>conou<CR><SID>:
cnoremap <silent>   <SID>conou	<C-\>e<sid>undo()
cnoremap <script>   <SID>:U	<SID>conoU<CR><SID>:
cnoremap <silent>   <SID>conoU	<C-\>e<sid>redo()

" Repeating: . {{{1
cnoremap <script>   <SID>:.	<SID>cono.<CR><SID>:
cnoremap <silent>   <SID>cono.	<C-\>e<sid>edit_dot()

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

" Omap Motions: h l w b $ {{{1
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

" Goodies: c_CTRL-R_* {{{1
" with undo, count, dot-repeat
cnoremap <script>   <SID>:<C-R>	<SID>"
cnoremap <script>   <SID>"*	<SID>CtlR*<CR><SID>:
cnoremap <silent>   <SID>CtlR*	<C-\>e<sid>edit_put(1,"*",0,0)
cmap		    <SID>"	<C-R>

" Mode Switching: {{{1
" From Cmdline-Normal mode 
" to Cmdline mode (start over)
cnoremap	    <SID>::	<C-E><C-U>

" to Cmdline mode
cnoremap	    <SID>:i	x<BS>
cnoremap	    <SID>:a	<Right>
cnoremap	    <SID>:I	<C-B>
cnoremap	    <SID>:A	<C-E>

" to Cmdline mode (key not mapped -> make <SID>: do nothing)
cnoremap <script>   <SID>:	<SID>rst_tm<CR>
cnoremap <silent>   <SID>rst_tm <C-R>=<sid>rst_tm()

" Cmdline-Omap mode to Cmdline-Normal mode (implicit)
cmap		    <SID>;	<SID>:
" maybe:
cnoremap <script>   <SID>;<Esc> <SID>:

"}}}1

" Modeline: {{{1
let &cpo = s:cpo_sav
" vim:set ts=8 sts=4 sw=4 fdm=marker:
