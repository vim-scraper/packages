" File: cscope_quickfix.vim
" Author: kino
" Version: 0.1
" Last Modified: Dec. 18, 2003
" 
" Overview
" --------
"
" Usage
" -----
" 
" Configuration
" -------------
"
if !exists("Cscope_OpenQuickfixWindow")
	let Cscope_OpenQuickfixWindow = 1
endif

if !exists("Cscope_JumpError")
	let Cscope_JumpError = 1
endif

" RunCscope()
" Run the cscope command using the supplied option and pattern
function! s:RunCscope(...)
	let usage = "Usage: Cscope {type} {pattern} [{file}]."
	let usage = usage . " {type} is [sgdctefi01234678]."
	if !exists("a:1") || !exists("a:2")
		echohl WarningMsg | echomsg usage | echohl None
		return
	endif
	let cscope_opt = a:1
	let pattern = a:2
	if cscope_opt == '0' || cscope_opt == 's'
		let cmd = "cscope -L -0 " . pattern
	elseif cscope_opt == '1' || cscope_opt == 'g'
		let cmd = "cscope -L -1 " . pattern
	elseif cscope_opt == '2' || cscope_opt == 'd'
		let cmd = "cscope -L -2 " . pattern
	elseif cscope_opt == '3' || cscope_opt == 'c'
		let cmd = "cscope -L -3 " . pattern
	elseif cscope_opt == '4' || cscope_opt == 't'
		let cmd = "cscope -L -4 " . pattern
	elseif cscope_opt == '6' || cscope_opt == 'e'
		let cmd = "cscope -L -6 " . pattern
	elseif cscope_opt == '7' || cscope_opt == 'f'
		let cmd = "cscope -L -7 " . pattern
	elseif cscope_opt == '8' || cscope_opt == 'i'
		let cmd = "cscope -L -8 " . pattern
	else
		echohl WarningMsg | echomsg usage | echohl None
		return
	endif
	if exists("a:3")
		let cmd = cmd . " " . a:3
	endif
	let cmd_output = system(cmd)

	if cmd_output == ""
		echohl WarningMsg | 
		\ echomsg "Error: Pattern " . pattern . " not found" | 
		\ echohl None
		return
	endif

	let tmpfile = tempname()

	exe "redir! > " . tmpfile
	silent echon cmd_output
	redir END

	let old_efm = &efm
	set efm=%f\ %*[^\ ]\ %l\ %m

	exe "silent! cfile " . tmpfile

	let &efm = old_efm

	" Open the cscope output window
	if g:Cscope_OpenQuickfixWindow == 1
		botright cwin
	endif

	" Jump to the first error
	if g:Cscope_JumpError == 1
		cc
	endif

	call delete(tmpfile)
endfunction

" Define the set of Cscope commands
command! -nargs=* Cscope call s:RunCscope(<f-args>)

nmap <C-\>s :Cscope s <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>g :Cscope g <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>d :Cscope d <C-R>=expand("<cword>")<CR> <C-R>=expand("%")<CR><CR>
nmap <C-\>c :Cscope c <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>t :Cscope t <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>e :Cscope e <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>f :Cscope f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-\>i :Cscope i ^<C-R>=expand("<cfile>")<CR>$<CR>

nmap <C-@>s :split<CR>:Cscope s <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>g :split<CR>:Cscope g <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>d :split<CR>:Cscope d <C-R>=expand("<cword>")<CR> <C-R>=expand("%")<CR><CR>
nmap <C-@>c :split<CR>:Cscope c <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>t :split<CR>:Cscope t <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>e :split<CR>:Cscope e <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>f :split<CR>:Cscope f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-@>i :split<CR>:Cscope i ^<C-R>=expand("<cfile>")<CR>$<CR>

nmap <C-@><C-@>s :vert split<CR>:Cscope s <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>g :vert split<CR>:Cscope g <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>d :vert split<CR>:Cscope d <C-R>=expand("<cword>")<CR> <C-R>=expand("%")<CR><CR>
nmap <C-@><C-@>c :vert split<CR>:Cscope c <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>t :vert split<CR>:Cscope t <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>e :vert split<CR>:Cscope e <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>f :vert split<CR>:Cscope f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-@><C-@>i :vert split<CR>:Cscope i ^<C-R>=expand("<cfile>")<CR>$<CR>

