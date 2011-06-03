" vim:set ts=8 sts=2 sw=2 tw=0:
"
" helpclose.vim - Close all help window
"
" Maintainer:	MURAOKA Taro <koron@tka.att.ne.jp>
" Last Change:	21-Mar-2003.
"
" Description:
" Command:
"   :HelpAllClose   Close all help windows.

function! s:HelpClose(...)
  " Save current window number to revert.
  let save_winnr = winnr()
  let nwin = 1
  while 1
    let nbuf = winbufnr(nwin)
    " After all window processed, finish.
    if nbuf == -1
      break
    endif
    " Close window if its buftype is help.  If not help, go to next window.
    if getbufvar(nbuf, '&buftype') ==# 'help'
      " Correct saved window number if younger window will be closed.
      if save_winnr > nbuf
	let save_winnr = save_winnr - 1
      endif
      execute nwin.'wincmd w'
      " If there is only one help window, quit.
      if nwin == 1 && winbufnr(2) == -1
	quit!
      else
	close!
      endif
    else
      let nwin = nwin + 1
    endif
  endwhile
  " Revert selected window.
  execute save_winnr.'wincmd w'
endfunction

command! -nargs=0 HelpAllClose call <SID>HelpClose()
