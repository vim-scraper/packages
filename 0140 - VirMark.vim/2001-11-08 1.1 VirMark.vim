" -*- vim -*-
" FILE: "/home/dp/.vim/VirMark.vim" {{{
" LAST MODIFICATION: "Thu, 08 Nov 2001 16:51:45 (dp)"
" (C) 2001 by Douglas L. Potts, <dlpotts@spectral-sys.com>
" Description:  Create a virtual 'mark' by saving the current line number and
"               column.
"               NOTE: only for regular non-caps type marks (at least for now).
"
" Autoload Usage:
"       For those using Johannes Zellner's autoload script set, uncomment the
"       line that has "used by the autoload generator" in it, and put this
"       in the autoload directory in your runtime path.
"
" $Id: VirMark.vim,v 1.1 2001/11/08 21:49:53 dp Exp $ }}}

if exists('g:VirMark_loaded') | finish | endif
let g:VirMark_loaded = 1

" Return 0 if mark operation went OK.  1 if problems.
function! VirMark(setMark, markChar)
  let s:ret_val = 1
  let s:setMark  = a:setMark
  let s:markChar = a:markChar

  " Is the markChar valid
  if s:markChar =~ '[a-z]'
    " setMark = 1 -> Set the mark,    setMark = 0 -> Go to the mark
    if s:setMark == 1
      " Line essentially does the following, but allows b:marka to be
      " b:mark*
      " let b:marka = line(".") . "normal" . col(".") . "|"
      exe "let b:mark".s:markChar."=line(\".\").\"normal\".col(\".\").\"|\""
      let s:ret_val = 0
    else
      " Check that mark exists
      if exists("b:mark".s:markChar)
        exe "exe b:mark".s:markChar
        let s:ret_val = 0
      endif
    endif
  endif
  return s:ret_val
endfunction

"if exists('g:autoload') | finish | endif " used by the autoload generator

command! -nargs=1 SetMark exe "call VirMark(1,\"".<args>."\")"
command! -nargs=1 GoMark  exe "call VirMark(0,\"".<args>."\")"
