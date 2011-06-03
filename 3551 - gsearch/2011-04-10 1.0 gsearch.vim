" Vim global plugin to search over Google inside Vim
" Last Change: 10 Apr 2011
" Maintainer: Saleh Alsanad <q8mosfet AT gmail DOT com>
" License: This file is placed in the public domain
" Example: :Google vim rocks
" You have to have text-based browser 'links' installed in your box
" available at http://links.twibright.com/

if exists("loaded_gsearch")
  finish
endif
let loaded_gsearch = 1

command -nargs=+ Google call GoogleSearch(<f-args>)

function GoogleSearch(...)
	let q = substitute(join(a:000, " "), ' ', "+", "g")
	exe '!links https://encrypted.google.com/search?q=' . q
endfunction
