if exists("g:matlab_open")==0
	let g:matlab_open=0
endif

function! Startmatlab()
	if g:matlab_open==0
		echo "Openning Matlab..."
		py from win32com.client import Dispatch
		py import vim
		py h=Dispatch('matlab.application')
		let g:matlab_open=1
		echo "Done."
	else
		echo "Matlab has already been opened!"
	endif
endfunction

function! Pathmatlab()
	if g:matlab_open==1
		py h.Execute("temppath='"+vim.eval('expand("%:p:h")')+"'")
		py h.Execute("cd(temppath)")
		py h.Execute("clear temppath")
		py h.Execute("rehash path")
		py print h.Execute(vim.eval('expand("%:t:r")'))
	else
		echo "Matlab is not opened."
	endif
endfunction


if has("win32") && has("python")
	map <silent> <buffer> ,o :call Startmatlab()<CR>
	map <silent> <buffer> ,r :w<CR>:call Pathmatlab()<CR>
endif



