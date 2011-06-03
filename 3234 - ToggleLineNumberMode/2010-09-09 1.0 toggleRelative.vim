function! g:ToggleNuMode()
	if(&rnu == 1)
		set nu
	else
		set rnu
	endif
endfunc

nnoremap <C-L> :call g:ToggleNuMode()<cr>
