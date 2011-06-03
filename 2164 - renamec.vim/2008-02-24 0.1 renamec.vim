" renamec.vim -- Simple function to rename C symbols using cscope
" @Author:      <bastian at mathes dot jp>
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-02-24.

function! Renamec()
	" store old buffer and restore later
	let stored_buffer = bufnr("%")
	
	" start refactoring
	let old_name = expand("<cword>")
	let new_name = input("new name: ",old_name)

	let cscope_out = system("cscope -L -d -F cscope.out -0 " . old_name)
	let cscope_out_list = split(cscope_out,'\n')
	
	for cscope_line in cscope_out_list
		let cscope_line_split = split(cscope_line,' ')
		let subs_file = cscope_line_split[0]
		let subs_lnr = cscope_line_split[2]
		let subs_buffer = bufnr(subs_file)

		if subs_buffer == -1
			exe "edit ".subs_file
			let do_close = 1
			let subs_buffer = bufnr(subs_file)
		else
			let do_close = 0
		endif

		if subs_buffer != -1
			exe "buffer ".subs_buffer
			exe subs_lnr.",".subs_lnr."s/".old_name."/".new_name."/g"
			exe "write"
			if do_close == 1
				exe "bd"
			endif
		endif
	endfor
	exe "buffer ".stored_buffer
endfunction
