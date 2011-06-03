" GreedyBackspace.vim  -  Make backspace eat multiple whitespace characters.
"
" Copyrright October 2005 by Christian J. Robinson <infynity@onewest.net>
"
" Distributed under the terms of the Vim license.  See ":help license".
"
" This script isn't very useful if you don't put
"   set backspace=indent,eol,start
" or at least
"   set backsapce=indent,start
" in your vimrc.
"
" You can do ":let g:greedybackspacenl = 1" to make this script eat newlines as
" well.

function! s:GreedyBackspace()
	let bs = ''

	let offset = s:ByteOffset() - 1
	let c = s:GetChar(offset)

	if c !~ '\s'
		return "\<BS>"
	endif

	while c =~ '\s' || exists('g:greedybackspacenl') && g:greedybackspacenl != 0 && c == "\0"
		let bs = bs . "\<BS>"
		let offset = offset - 1
		if (offset <= 1)
			break
		endif
		let c = s:GetChar(offset)
	endwhile

	return bs
endfunction

function! s:ByteOffset()
        return line2byte(line(".")) + col(".") - 1
endfunction

function! s:GetChar(offset)
	let line = byte2line(a:offset)
	let char = a:offset - line2byte(line)
	let c = getline(line)[char]
	return c
endfunction

" The 'smarttab' option can cause problems, so just disable it while
" backspacing:
function! s:ST(t)
  if a:t == 0
    let s:savesta=&sta | let &l:sta=0
  else
    let &l:sta=s:savesta | unlet s:savesta
  endif
  return ''
endfunction

inoremap <silent> <BS> <C-R>=<SID>ST(0)<CR><C-R>=<SID>GreedyBackspace()<CR><C-R>=<SID>ST(1)<CR>
