" Hebrew keyboard map
" 'QWERTY-phonetic/visual' kbd, for ISO-8859 like modes
" 'redundant' mode, ie, both p/f are map'd to ô, etc...
" (alef->224 ... no yiddish/punc/nikud )
" By: Oyd11 ( oyd11@softhome.net / http://oyd11.planet-d.net )
" Creation date: 26 dec 2003

" Usage:
" :so heb1.il.vim
" use '^_' for switching lang
" Features:
" + easy map configuration
" + a flag for selecting wherther or not to use 'revins'

"" XXX: buggy!!! 
"" BUGS:
""       * on calling funcs location is lost, this is undesirable
""       * on map is not local to buff... is this the way we want it?
""         can we do otherwise?
""      ?? can we make 'internal' 'hkmap' use our map simply?
""       * not very clean...
""       * this is like 'heb1.il.vim' only with different kbdmap
""         a certain 'integration' is req, also with other langs maybe?

let g:heb_rtl=1
" 0 => ltr, ie. don't do 'revins'
" 1 => rtl, ie, do 'revins' in mode switching
" affects only switching of 'revins' mode
" '0' is used mostly for my 'mirror-hebrew' mode proboably

let g:heb_is_on=0 " init val

function! HebSwitch()
 if g:heb_is_on==0
	execute MapHeb()
 else
	execute UnMapHeb()
 endif
endfunction

 map   mT:execute HebSwitch()'T
" ^ is there a 'cleaner' way to do so??
" ie: exe HebSwitch w/o losing pos?
 imap  A
 
" Map keys only on insert mode: ( imap / iunmap )
function! MapHeb()
 let g:heb_is_on=1
 if g:heb_rtl==1
	 set revins
 endif
 imap q ö 
 imap Q õ
" ^^ nothing left to match
 imap a à
 imap z æ
 imap w ù
 imap s ñ
 imap x ç
 imap e à
 imap d ã
 imap c ë
 imap C ê
 imap r ø
 imap f ô
 imap F ó
 imap v å
 imap t è
 imap T ú
 imap g â
 imap b á
 imap y ò
 imap h ä
 imap n ð
 imap N ï
 imap u å
 imap j é
 imap m î
 imap M í
 imap i é
 imap k ÷
 imap o å
 imap l ì
 imap p ô
 imap P ó
endfunction

function! UnMapHeb()
 let g:heb_is_on=0
 if g:heb_rtl==1
	 set norevins
 endif
 iunmap w
 iunmap W
 iunmap a
 iunmap z
 iunmap w
 iunmap s
 iunmap x
 iunmap e
 iunmap d
 iunmap c
 iunmap C
 iunmap r
 iunmap f
 iunmap F
 iunmap v
 iunmap t
 iunmap T
 iunmap g
 iunmap b
 iunmap y
 iunmap h
 iunmap n
 iunmap N
 iunmap u
 iunmap j
 iunmap m
 iunmap M
 iunmap i
 iunmap k
 iunmap o
 iunmap l
 iunmap p
 iunmap P
endfunction

