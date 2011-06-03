vmap ,m <esc>:call SCC()<cr>

" caracter that mean 'the cursor'
let g:SCC_cursor ='|'
let g:SCC_file ='C:\_projects\vimtest.vim'

function! SCC()
    " this function use @z and restore it a the end
    let l:save_z = @z

    "define the visual selection as workspace

    let l:line_min = line("'<")
    let l:line_max = line("'>")
    normal `<

    " search the cursor in the workspace
    " gives a message if no cursor found

    let l:line_cursor = search(g:SCC_cursor,"W")
    if l:line_cursor < l:line_min
	echo 'no cursor found in this string :('
    else
	" grab informations about the line where the cursor is
	
	let l:cursor_pos = col(".")
	let l:cursor_eol = col("$")

	" compute number of k and h to add to the mappings
	" to place the cursor at the right space
	
	let l:how_many_k = l:line_max - l:line_cursor
	let l:how_many_h = l:cursor_eol - l:cursor_pos - 1

	" if the cursor is the only one caracter of the string ..
	normal ^
	if l:cursor_pos == col(".")

	    " moves at the good line and add new line
	    let l:string_end = '<esc>' . l:how_many_k . 'ko'

	    " copy the visual selection in @z
	    normal gv"zd

	    " replace end of lines by the mapping
	    let @z = substitute(@z,"\n\[\t \]*","<cr>","g") . l:string_end

	    " replace the line where the cursor is
	    let @z = substitute(@z,"<cr>" . g:SCC_cursor . "<cr>","<cr>","g")
	else
	    " else : add good number of k and h
	    if l:how_many_h > 0
		let l:string_end = '<esc>' . l:how_many_k . 'k$' . l:how_many_h . 'hs'
	    else
		let l:string_end = '<esc>' . l:how_many_k . 'k$s'
	    endif
	    normal gv"zy
	    let @z = substitute(@z,"\n\[\t \]*","<cr>","g") . l:string_end
	endif

	" try to escape special caracters ...
	" is there a function to escape all special caracters ? check
	" vim.sf.net !

	let @z = substitute(@z,"\|\\|\\","\\\\&","g")
	normal "zp0
	let @z = l:save_z
    endif
endfunction


