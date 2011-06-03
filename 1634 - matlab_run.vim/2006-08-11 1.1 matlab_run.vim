function! Startmatlab()
	py from win32com.client import Dispatch
	py import vim
	py h=Dispatch('matlab.application')
endfunction

function! Pathmatlab()
	py h.Execute("temppath='"+vim.eval('expand("%:p:h")')+"'")
	py h.Execute("cd(temppath)")
	py h.Execute("clear temppath")
	py h.Execute("rehash path")
	py print h.Execute(vim.eval('expand("%:t:r")'))
endfunction

if has("win32") && has("python")
	call Startmatlab()
	map <silent> <buffer> ,r :w<CR>:call Pathmatlab()<CR>
endif


