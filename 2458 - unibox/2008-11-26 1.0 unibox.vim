" unibox.vim - draw styled line boxes in all languages.
" Copyright (C) 2008, Garrison Yu
" ---------------------------------------------------------------
" Version:       1.00
" Maintainer:    Garrison Yu <GarrisonZxYu dot vim at gmail dot com>
" Authors:       Garrison Yu <GarrisonZxYu dot vim at gmail dot com>
" Last Modified: 2008 Nov 25
" Based On:      Draw.vim (author: Timo Frenay)
" Created:       2008-11-25
" Homepage:      http://www.vim.org/scripts/script.php?script_id=2458
"
"
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of the License, or
" (at your option) any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
"
" ---------------------------------------------------------------
" Overview
" ---------------------------------------------------------------
" 
" You can get all kinds of lines supported in your local system.  For Chinese,
" Japanese, and Korean (CJK), the program will be fully featured with various
" pre-defined lines.  Other encoding will have degraded lines.  For example, if
" you have not 3-dot line in your system, a solid line will be used.  In the
" worst case, characters (+,-,|) will be applied.
"
" You can draw the following styles of lines under the virtual edit mode
" (block or all).
"     thin lines, bold lines
"     thin 3-dot lines, bold 3-dot lines
"     thin 4-dot lines, bold 4-dot lines
"     double lines
"     and round corners
" If lines cross, the program will use the suitable lines to draw the box.
" You can also use unite mode or overwrite mode to control how lines are
" intersected.
"
" Note: the actual line styles depends on your encoding and fonts.
"
" In case a visual selection cover an existing box, there are 2 modes for
" processing the intersection, unite mode and overwrite mode.  In unite mode,
" lines are joined at intersections. This is why it is called unibox.
" 
" ---------------------------------------------------------------
" Key mappings:
" ---------------------------------------------------------------
" 
"     <leader>du    uses it after you change you encoding
"     <leader>d     the default draw mode, same as <leader>d1
"     <leader>d1    draws with thin lines
"     <leader>d2     with bold lines
"     <leader>d3     with 3-dot thin lines
"     <leader>d4     with 4-dot thin lines
"     <leader>d5     with double lines
"     <leader>d6     with 3-dot bold lines
"     <leader>dr     sam as <leader>dr1
"     <leader>dr1    with round corners and thin lines
"     <leader>dr3    with round corners and 3-dot thin lines
"     <leader>dr4    with round corners and 4-dot thin lines
"     <leader>d8     with round corners and 4-dot bold lines
"     <leader>dc    clear the box
" 
"     <leader>dd    The followings correspond the above but in a overwrite mode
"     <leader>dd1  
"     <leader>dd2  
"     <leader>dd3  
"     <leader>dd4  
"     <leader>dd5  
"     <leader>dd6  
"     <leader>ddr  
"     <leader>ddr1  
"     <leader>ddr3  
"     <leader>ddr4  
"     <leader>dd8  
"     <leader>ddC  
" 
" ---------------------------------------------------------------
" Tips:
" ---------------------------------------------------------------
" 
" 1.Try the following if you encounter wierd lines:
"         set ambiwith=single
"   or   set ambiwith=double
" 
" 2.To change the encoding, use
"         set enc=...
"   and type
"         <leader>du
"   to reset the drawing system.
"  
" 3. In most cases, utf-8 encoding will get a better results.
"  
"  
" ---------------------------------------------------------------
" Install details
" ---------------------------------------------------------------
"  
" Copy the file to your favorite directory and source it when you need it.
" For example, in vim, use command:
"   :so unibox.vim
"
" ---------------------------------------------------------------


function! <SID>Init()
    " Init the internal char tables using unicode characters.
    "
    " line style codes, each char is represented by for digits
    " 1 -    normal line
    " 2 _    bold   line
    " 3 ...  3-dot  line
    " 4 .... 4-dot  line
    " 5 =    double line
    " 6 :::  bold 3 dot line
    " 7 c    circle-corner line
    " 8 :::: bold 4-dot line
     let s:basLineType = {
		 \ 'hh': '0011', 'vr': '1101', 'vv': '1100',
		 \ 'dl': '0110', 'dr': '0101', 'uh': '1011',
		 \ 'vl': '1110', 'ur': '1001', 'dh': '0111',
		 \ 'ul': '1010', 'vh': '1111'}

    let dig2chr_unicode = {
		\  '0011' : 9472,
		\  '0022' : 9473,
		\  '0033' : 9476,
		\  '0044' : 9480,
		\  '0055' : 9552,
		\  '0066' : 9477,
		\  '0088' : 9481,
		\  '0101' : 9484,
		\  '0102' : 9485,
		\  '0105' : 9554,
		\  '0110' : 9488,
		\  '0111' : 9516,
		\  '0112' : 9518,
		\  '0120' : 9489,
		\  '0121' : 9517,
		\  '0122' : 9519,
		\  '0150' : 9557,
		\  '0155' : 9572,
		\  '0201' : 9486,
		\  '0202' : 9487,
		\  '0210' : 9490,
		\  '0211' : 9520,
		\  '0212' : 9522,
		\  '0220' : 9491,
		\  '0221' : 9521,
		\  '0222' : 9523,
		\  '0501' : 9555,
		\  '0505' : 9556,
		\  '0510' : 9558,
		\  '0511' : 9573,
		\  '0550' : 9559,
		\  '0555' : 9574,
		\  '0707' : 9581,
		\  '0770' : 9582,
		\  '1001' : 9492,
		\  '1002' : 9493,
		\  '1005' : 9560,
		\  '1010' : 9496,
		\  '1011' : 9524,
		\  '1012' : 9526,
		\  '1020' : 9497,
		\  '1021' : 9525,
		\  '1022' : 9527,
		\  '1050' : 9563,
		\  '1055' : 9575,
		\  '1100' : 9474,
		\  '1101' : 9500,
		\  '1102' : 9501,
		\  '1105' : 9566,
		\  '1110' : 9508,
		\  '1111' : 9532,
		\  '1112' : 9534,
		\  '1120' : 9509,
		\  '1121' : 9533,
		\  '1122' : 9535,
		\  '1150' : 9569,
		\  '1155' : 9578,
		\  '1201' : 9503,
		\  '1202' : 9506,
		\  '1210' : 9511,
		\  '1211' : 9537,
		\  '1212' : 9542,
		\  '1220' : 9514,
		\  '1221' : 9541,
		\  '1222' : 9544,
		\  '2001' : 9494,
		\  '2002' : 9495,
		\  '2010' : 9498,
		\  '2011' : 9528,
		\  '2012' : 9530,
		\  '2020' : 9499,
		\  '2021' : 9529,
		\  '2022' : 9531,
		\  '2101' : 9502,
		\  '2102' : 9505,
		\  '2110' : 9510,
		\  '2111' : 9536,
		\  '2112' : 9540,
		\  '2120' : 9513,
		\  '2121' : 9539,
		\  '2122' : 9543,
		\  '2200' : 9475,
		\  '2201' : 9504,
		\  '2202' : 9507,
		\  '2210' : 9512,
		\  '2211' : 9538,
		\  '2212' : 9546,
		\  '2220' : 9515,
		\  '2221' : 9545,
		\  '2222' : 9547,
		\  '3300' : 9478,
		\  '4400' : 9482,
		\  '5001' : 9561,
		\  '5005' : 9562,
		\  '5010' : 9564,
		\  '5011' : 9576,
		\  '5050' : 9565,
		\  '5055' : 9577,
		\  '5500' : 9553,
		\  '5501' : 9567,
		\  '5505' : 9568,
		\  '5510' : 9570,
		\  '5511' : 9579,
		\  '5550' : 9571,
		\  '5555' : 9580,
		\  '6600' : 9479,
		\  '7007' : 9584,
		\  '7070' : 9583,
		\  '8800' : 9483,}

    "convert unicode to user's encoding
    let s:dig2chr = {}
    let old_enc = &enc
    let &enc = 'utf-8'
    for key in keys(dig2chr_unicode)
	let chr = iconv(nr2char(dig2chr_unicode[key]), 'utf-8', old_enc)
	if empty(chr)
	    let chr = ' '
	endif
	let s:dig2chr[key] = chr
    endfor
    let &enc = old_enc


    "replace the missing characters
    "1.scan for non-existing, but can be replace by 2, or 1
    for key in keys(s:dig2chr)
	if match(s:dig2chr[key], '?') >= 0

	    " if char not found, use the following sequence to reduce the line
	    " attributes:
	    "   8 -> 2
	    "   7 -> 1
	    "   6 -> 2
	    "   5 -> 2
	    "   4 -> 1
	    "   3 -> 1
	    let k = tr(key, '876543','212211')
	    let s:dig2chr[key] = s:dig2chr[k]
	endif
    endfor
    "2.scan non-existing char for 22 (|, or --), which is a mix of bold and thin
    "lines
    for key in keys(s:dig2chr)
	if match(s:dig2chr[key], '?') >= 0
	    let k = tr(key, '876543','212211')
	    let k = substitute(k, '^22\|22$', '11','g') "  22 -> 11
	    let s:dig2chr[key] = s:dig2chr[k]
	endif
    endfor
    "3.if system has no bold lines, changed them to thin lines
    for key in keys(s:dig2chr)
	if match(s:dig2chr[key], '?') >= 0
	    let k = tr(key, '876543','212211')
	    let k = substitute(k, '2', '1','g')   "  2 -> 1
	    let s:dig2chr[key] = s:dig2chr[k]
	endif
    endfor
    "4.if still not exist thin lines, reduce to only ascii char: +-|
    for key in keys(s:dig2chr)

	if match(s:dig2chr[key], '?') >= 0 || s:dig2chr[key] == nr2char(0xbf)
	    let k = substitute(key, '[^0]', 'x', 'g')
	    if k == 'xxxx'
		let k = '+'
	    elseif k[0:1] == 'xx'
		let k = '|'
	    elseif k[2:3] == 'xx'
		let k = '-'
	    else
		let k = '+'
	    endif
	    let s:dig2chr[key] = k
	endif
    endfor

    "set up chr2dig
    let s:chr2dig = {}
    for key in keys(s:dig2chr)
	let chr = s:dig2chr[key]
	let s:chr2dig[chr] = key
    endfor

    "set up the lineTypes
    let s:lineTypes = {}
    for key in keys(s:basLineType)
	let codes = []
	let i = 1
	while i <= 10
	    let chr = s:basLineType[key]
	    let chr = substitute(chr, '[^0]', printf('%x',i), 'g')
	    let chr = CharNormalize(chr)
	    call add(codes, chr)
	    let i = i + 1
	endwhile
	let s:lineTypes[key] = codes
    endfor

    let s:charwidth = s:GetCharWidth(s:dig2chr['0011'][0])
    let s:dig2chr['0000'] = '    '[:(s:charwidth-1)]
    let s:chr2dig[char2nr(' ')] = '  '
endfunction

function! CharNormalize(chr)
    " check to see if a char exists. if not, replace it with a similar one
    "
    let chr = a:chr
    if exists('s:dig2chr['''.chr.''']')
	return chr
    endif

    let chr = substitute(chr, '^99\|99$', '33','g') "  99 -> 33
    let chr = substitute(chr, '^aa\|aa$', '44','g') "  aa -> 44
    let chr = tr(chr, '9a','77')
    if exists('s:dig2chr['''.chr.''']')
	return chr
    endif

    let chr = tr(chr, '876543','212211')
    if exists('s:dig2chr['''.chr.''']')
	return chr
    endif

    let chr = substitute(chr, '^22\|22$', '11','g') "  22 -> 11
    if exists('s:dig2chr['''.chr.''']')
	return chr
    endif

    let chr = substitute(chr, '2', '1','g')   "  2 -> 1
    if exists('s:dig2chr['''.chr.''']')
	return chr
    endif

    return "error"
endfunction

function! CharBind(chr1, chr2,...)
    if ! exists("s:linemode")
	let s:linemode = 'or'
    endif
    if a:0 > 0
	let s:linemode = tolower(a:1)
    endif

    if s:linemode == 'or'
	return CharOr(a:chr1, a:chr2)
    elseif s:linemode == 'overwrite'
	return a:chr1
    elseif s:linemode == 'clear'
	return CharClear(a:chr1, a:chr2)
    else
	"wrong mode in combining chars: s:linemode
	"using default or mode
	return CharOr(a:chr1, a:chr2)
    endif

endfunction

function! CharClear(chr1, chr2,...)
    let c1 = a:chr1
    let c2 = a:chr2
    if len(c1) != 4 || len(c2) !=4
	return "error"
    endif

    let res = ''
    let res .= c1[0] ? '0' : c2[0]
    let res .= c1[1] ? '0' : c2[1]
    let res .= c1[2] ? '0' : c2[2]
    let res .= c1[3] ? '0' : c2[3]

    let res = CharNormalize(res)
    if res == 'error'
	return '0000'
    endif
    return res
endfunction

function! CharOr(chr1, chr2)
    let c1 = a:chr1
    let c2 = a:chr2
    if len(c1) != 4 || len(c2) !=4
	return "error"
    endif

    let res = ''
    let res .= c1[0] ? c1[0] : c2[0]
    let res .= c1[1] ? c1[1] : c2[1]
    let res .= c1[2] ? c1[2] : c2[2]
    let res .= c1[3] ? c1[3] : c2[3]

    let res = CharNormalize(res)
    return res

endfunction

function! <SID>DrawRect(ltype, roundCorner)
  let l:ltype = a:ltype
  if l:ltype == 9
      let s:linemode = 'clear'
      let l:ltype = 0
  elseif l:ltype >= 10
      let s:linemode = 'overwrite'
      let l:ltype -= 10
  else
      let s:linemode = 'or'
  endif

  if a:roundCorner == 'r'
      if l:ltype == 0
	  let l:ltype = 6
      elseif l:ltype == 2
	  let l:ltype = 8
      elseif l:ltype == 3
	  let l:ltype = 9
      endif
  endif

  if (visualmode() != "\<C-V>")
    " Beep!
    execute "normal! \<Esc>"
    echoerr "DrawRect() requires a blockwise Visual selection"
    return
  endif
  " Backup options.
  let l:virtualedit = &virtualedit
  let l:wrap = &wrap
  "let l:charwidth = s:GetCharWidth(s:dig2chr['hh'][0])
  let l:charwidth = s:charwidth
  " Set options.
  set virtualedit=all nowrap
  " Get rectangle boundaries.
  let l:top = line("'<")
  let l:left = virtcol("'<")
  let l:bottom = line("'>")
  let l:right = virtcol("'>")
  if (l:top == l:bottom)
    normal! `<
    if (l:left == l:right)
      " Draw a cross. I hope that's what you wanted...
      call <SID>DrawCode(s:lineTypes['vh'][l:ltype])
    else 
      " Draw a horizontal line.
      call <SID>DrawCode(s:lineTypes['hh'][l:ltype])
      let l:count = l:right - l:left - 2*l:charwidth
      while (l:count>0)
        normal! l
        call <SID>DrawCode(s:lineTypes['hh'][l:ltype])
        let l:count = l:count - l:charwidth
      endwhile
      normal! l
      call <SID>DrawCode(s:lineTypes['hh'][l:ltype])
    endif
  elseif (l:left == l:right)
    " Draw a vertical line.
    normal! `<
    call <SID>DrawCode(s:lineTypes['vv'][l:ltype])
    let l:count = l:bottom - l:top - 1
    while (l:count>0)
      normal! j
      call <SID>DrawCode(s:lineTypes['vv'][l:ltype])
      let l:count = l:count - 1
    endwhile
    normal! j
    call <SID>DrawCode(s:lineTypes['vv'][l:ltype])
  else
    " Draw a rectangle.
    let pos_switched = 0
    if (l:right < l:left)
      " Blockwise Visual selection is right-to-left, mirror it first.
      execute "normal! gvO\<Esc>"
      let l:left = virtcol("'<")
      let l:right = virtcol("'>")
      let pos_switched = 1
    endif
    normal! `<j
    " Draw left side.
    let l:count = l:bottom - l:top - 1
    while (l:count>0)
      call <SID>DrawCode(s:lineTypes['vv'][l:ltype])
      let l:count = l:count - 1
      normal! j
    endwhile
    " Draw bottom-left corner.
    call <SID>DrawCode(s:lineTypes['ur'][l:ltype])
    " Draw bottom side.
    let l:count = l:right - l:left - 2*l:charwidth
    while (l:count>0)
      normal! l
      call <SID>DrawCode(s:lineTypes['hh'][l:ltype])
      let l:count = l:count - l:charwidth
    endwhile
    " Draw bottom-right corner.
    normal! l
    call <SID>DrawCode(s:lineTypes['ul'][l:ltype])
    " Draw top-left corner.
    normal! `<
    call <SID>DrawCode(s:lineTypes['dr'][l:ltype])
    " Draw top side.
    let l:count = l:right - l:left - 2*l:charwidth
    while (l:count>0)
      normal! l
      call <SID>DrawCode(s:lineTypes['hh'][l:ltype])
      let l:count = l:count - l:charwidth
    endwhile
    " Draw top-right corner.
    normal! l
    call <SID>DrawCode(s:lineTypes['dl'][l:ltype])
    " Draw right side.
    let l:count = l:bottom - l:top - 1
    while (l:count>0)
      normal! j
      call <SID>DrawCode(s:lineTypes['vv'][l:ltype])
      let l:count = l:count - 1
    endwhile

    " Return the cursor to the end of the Visual selection.
    if pos_switched
      execute "normal! gvO\<Esc>"
    endif
    normal! `>
  endif
  " Restore options.
  let &virtualedit = l:virtualedit
  let &wrap = l:wrap
endfunction

" Draw the boxdrawing character with the specified drawing code at the cursor
" position, combining it with any boxdrawing character under the cursor.
function! <SID>DrawCode(code)
  if index(keys(s:dig2chr), a:code) == -1
    " Beep!
    "execute "normal! \<Esc>"
    "echoerr "Invalid drawing code:" a:code
    return
  endif
  " Backup "c register.
  let l:c = @c
  " Get the character under the cursor.
  normal! "cyl
  let l:char = @c

  if l:char == '' || index(keys(s:chr2dig),(l:char)."") == -1
      "let l:char = a:code
      let chr2 = '0000'
  else
      let chr2 = s:chr2dig[(l:char).""] 
  endif
  let l:char = CharBind(a:code, chr2)

  if chr2 == l:char
      " no need to draw
      return
  endif
  "for overwrite mode
  "let l:char = a:code
  " Replace the character under the cursor.
  "execute "normal! r" . s:dig2chr[l:char][0]
  let l:bytes = s:GetCharWidth('')
  let l:bytes = l:bytes == 0 ? 1 : l:bytes
  "let l:offset = s:GetCharWidth(s:dig2chr[l:char][0]) / l:bytes 
  let l:offset = s:charwidth / l:bytes 
  let l:offset = l:offset == 0 ? 1 : l:offset

  execute "normal! c".l:offset."l" . s:dig2chr[l:char]
  "echo char2nr(l:char)
  " Restore "c register.
  let @c = l:c
endfunction

function! s:GetCharWidth(ch)
    " get column width of the given char or the char under the cursor

    "insert the given char if not null
    let l:modifiable = &modifiable
    if !empty(a:ch)
	let &modifiable = 1
	execute 'normal! i'.a:ch
    endif
    "get the character under cursor
    let l:ve_bak = &ve
    let l:c = @c
    normal! "cyl
    let l:chr = @c
    let @c = l:c

    if char2nr(l:chr) == 9
	" special case for <tab>
	if match(l:ve_bak,'insert\|all') >= 0
	    let l:width = 1
	else
	    let l:width = &ts
	endif
    else
	"measure the lenth of the char
	set ve=all
	let c1 = virtcol('.')
	normal! h
	let c2 = virtcol('.')
	if c1 == c2
	    let width = c1
	else
	    normal! l
	    let width = c1 - c2
	endif
	let &ve=l:ve_bak

	if l:width <= 0
	    let l:width = 1
	endif
    endif
    "resume the change
    if !empty(a:ch)
	execute 'silent! u'
	let &modifiable = l:modifiable
    endif
    return l:width
endfunction

	        

function! GetCharWidth(ch)
    return s:GetCharWidth(a:ch)
endfunction


call <SID>Init()
let dig2chr = s:dig2chr
let chr2dig = s:chr2dig
let lineTypes = s:lineTypes
"let lineMode = s:linemode



nnoremap <silent> <leader>du    <Esc>:call <SID>Init()<CR>
vnoremap <silent> <leader>d     <Esc>:call <SID>DrawRect(0,'')<CR>
vnoremap <silent> <leader>d1    <Esc>:call <SID>DrawRect(0,'')<CR>
vnoremap <silent> <leader>d2    <Esc>:call <SID>DrawRect(1,'')<CR>
vnoremap <silent> <leader>d3    <Esc>:call <SID>DrawRect(2,'')<CR>
vnoremap <silent> <leader>d4    <Esc>:call <SID>DrawRect(3,'')<CR>
vnoremap <silent> <leader>d5    <Esc>:call <SID>DrawRect(4,'')<CR>
vnoremap <silent> <leader>d6    <Esc>:call <SID>DrawRect(5,'')<CR>
vnoremap <silent> <leader>dr    <Esc>:call <SID>DrawRect(0,'r')<CR>
vnoremap <silent> <leader>dr1   <Esc>:call <SID>DrawRect(0,'r')<CR>
vnoremap <silent> <leader>dr3   <Esc>:call <SID>DrawRect(2,'r')<CR>
vnoremap <silent> <leader>dr4   <Esc>:call <SID>DrawRect(3,'r')<CR>
vnoremap <silent> <leader>d8    <Esc>:call <SID>DrawRect(7,'')<CR>
vnoremap <silent> <leader>dc    <Esc>:call <SID>DrawRect(9,'')<CR>
vnoremap <silent> <leader>dd    <Esc>:call <SID>DrawRect(10,'')<CR>
vnoremap <silent> <leader>dd1   <Esc>:call <SID>DrawRect(11,'')<CR>
vnoremap <silent> <leader>dd2   <Esc>:call <SID>DrawRect(12,'')<CR>
vnoremap <silent> <leader>dd3   <Esc>:call <SID>DrawRect(13,'')<CR>
vnoremap <silent> <leader>dd4   <Esc>:call <SID>DrawRect(14,'')<CR>
vnoremap <silent> <leader>dd5   <Esc>:call <SID>DrawRect(15,'')<CR>
vnoremap <silent> <leader>dd6   <Esc>:call <SID>DrawRect(16,'')<CR>
vnoremap <silent> <leader>ddr   <Esc>:call <SID>DrawRect(10,'r')<CR>
vnoremap <silent> <leader>ddr1  <Esc>:call <SID>DrawRect(10,'r')<CR>
vnoremap <silent> <leader>ddr3  <Esc>:call <SID>DrawRect(12,'r')<CR>
vnoremap <silent> <leader>ddr4  <Esc>:call <SID>DrawRect(13,'r')<CR>
vnoremap <silent> <leader>dd8   <Esc>:call <SID>DrawRect(18,'')<CR>
vnoremap <silent> <leader>ddC   <Esc>:call <SID>DrawRect(9,'')<CR>


    " vim: sw=4: sts=4

