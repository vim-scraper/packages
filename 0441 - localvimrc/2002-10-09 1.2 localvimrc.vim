" Name: localvimrc.vim
" Version: 1.2
" Author: Markus Braun
" Date: 20021009
" $Id: localvimrc.vim,v 1.2 2002/10/09 11:17:46 mb Exp $
" Description: Search local vimrc files (".lvimrc") in the tree (root dir
" up to current dir) and load them.
" Installation: put this file into your plugin directory (~/.vim/plugin)
if exists("loaded_localvimrc")
	finish
end
let loaded_localvimrc = 1

function! Localvimrc() 
	let path = expand("%:p:h")
	if path == ""
		let path = getcwd()
	endif
	let path = path . "/"
	let currpath = "/"

	while 1
		let filename = currpath . ".lvimrc"
		if filereadable(filename)
			exec 'source ' . filename
			"echo 'Loaded ' . filename
		endif
		if path == currpath
			break
		endif
		let pos = matchend(path, "/", strlen(currpath))
		let currpath = strpart(path, 0, pos)
	endwhile
endfunction

" Call Localvimrc() when loading this plugin
if has("autocmd")
	autocmd BufNewFile,BufRead * call Localvimrc()
endif
" vim600:fdm=marker:commentstring="\ %s:
