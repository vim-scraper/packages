" Hebrew keyboard map
" 'israeli/QWERTY' kbd, for ISO-8859 like modes
" (alef->224 ... no yiddish/punc/nikud ) setup, but:
"  + with shift -> sejfa-letters, etc
"  + no-punc-overrides!
" By: Oyd11 ( oyd11@softhome.net / http://oyd11.planet-d.net )
" Creation date: 26 dec 2003

" Usage:
" :so heb1.il.vim
" use '^_' for switching lang
" Features:
" + easy map configuration
" + a flag for selecting wherther or not to use 'revins'
" Mapping:
" all letters, as printed on Israelli keyboards, except:
"  + each finial-letter, is just it's normal letter shifted
"  + original keys with finial letters are left un-mapped
"  + an exception is 'taf' (�), which is mapped on shift-tet (�)
"    thus, also the ',' key is free
"  + as a result, all normal punctuation keys, are left unremapp'd

"" XXX: buggy!!! 
"" BUGS:
""       * on calling funcs location is lost, this is undesirable
""       * on map is not local to buff... is this the way we want it?
""         can we do otherwise?
""      ?? can we make 'internal' 'hkmap' use our map simply?
""       * not very clean...

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
" imap q " NO-REMAP
 imap a �
 imap z �
" imap w " NO-REMAP
 imap s �
 imap x �
 imap e �
 imap d �
 imap c �
 imap r �
 imap f �
 imap F �
" 
 imap v �
 imap t �
 imap g �
 imap b �
 imap B �
 " INSTADE OF 'I'
 imap y �
 imap Y �
 " INSTADE OF ','
 imap h �
 imap n �
 imap N �
 " INSTADE OF 'O'
 imap u �
 imap j �
 imap m �
 imap M �
" INSTADE OF '.'
" imap i " NOMAP
 imap k �
" imap o " NOMAP
" imap l " NOMAP
 imap p �
 imap P �
endfunction

function! UnMapHeb()
 let g:heb_is_on=0
 if g:heb_rtl==1
	 set norevins
 endif
 iunmap a
 iunmap z
 iunmap s
 iunmap x
 iunmap e
 iunmap d
 iunmap c
 iunmap r
 iunmap f
 iunmap F
 iunmap v
 iunmap t
 iunmap g
 iunmap b
 iunmap B
 iunmap y
 iunmap Y
 iunmap h
 iunmap n
 iunmap N
 iunmap u
 iunmap j
 iunmap m
 iunmap M
 iunmap k
 iunmap p
 iunmap P
endfunction

