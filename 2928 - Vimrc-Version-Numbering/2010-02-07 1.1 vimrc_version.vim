"vimrc <> last updated :  2010/02/08 0:01:01
:let g:vimrcversion=1
:let g:debugmode=0

:au BufWritePre .vimrc,_vimrc :normal mx
:au BufWritePre .vimrc,_vimrc :normal Mmy
:au BufWritePre .vimrc,_vimrc normal :if &mod :%s/^\(".*last update[^:]*:\s*\).*/\=submatch(1).strftime("%c")/:endif
:au BufWritePre .vimrc,_vimrc :g/^:let g:debugmode=\d\+/let g:debugmode=matchstr(getline(line(".")), "\\d\\+")
:au BufWritePre .vimrc,_vimrc :normal :if g:debugmode==0 :if &mod  :%s/^\(:let g:vimrcversion=\)\([0-9\.]*\)\s*$/\=submatch(1).(submatch(2)+1)/:endif:endif
:au BufWritePre .vimrc,_vimrc :normal 'y
:au BufWritePre .vimrc,_vimrc :normal 'x




:au VimEnter * echo "VIMRC v"g:vimrcversion
