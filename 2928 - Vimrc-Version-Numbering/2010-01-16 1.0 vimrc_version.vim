",it	: (i)nsert (t)ime and date
"---------------------------------------------------------------------
:map ,it :let @a=strftime("%c")"ap

"vimrc <> last updated :  2010/01/16 15:02:51
:let g:vimrcversion=1
:au BufWritePre .vimrc,_vimrc :normal mx
:au BufWritePre .vimrc,_vimrc :normal Mmy
:au BufWritePre .vimrc,_vimrc :normal :if &mod :g/^"vimrc[^<]*[<][>] last update/normal :s/\(last update[^:]*:\).*/\1  /e:normal $,it:endif
:au BufWritePre .vimrc,_vimrc :normal :if &mod  :g/^:let g:vimrcversion=[0-9\.]*\s*$/normal :s/[0-9\.]\+/\=submatch(0)+1/:endif
"normal 0i /last update/:a  v$hx,it0x
:au BufWritePre .vimrc,_vimrc :normal 'y
:au BufWritePre .vimrc,_vimrc :normal 'x
:au VimEnter * echo "VIMRC v"g:vimrcversion
