"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""" This is vimsplit function to split among many files even :sp *.cpp<cr> works!! """"""""""
"""""""""""""""""""""""""""" Function written by Gael Induni """"""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""" Version 1.1 """"""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""" On February 9, 2010 """""""""""""""""""""""""""""""""""""""""""""""""
""" Inspired from http://vim.wikia.com/wiki/Opening_multiple_files_from_a_single_command-line """
"""""""""""""""" By salmanhalim """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! Spit(...)
	let l:sp = 'split'
	if a:0 == 0
		execute l:sp
	else
		" Put all the arguments in a list, in case it has got a list already
		if a:0 == 1 && type( a:1 ) == 3
			" Type == 3 is a list
			let l:myargs = a:1
		else
			let l:myargs = a:000
		endif
		"if a:1 == "--help"
		if l:myargs[ 0 ] == "--help"
			echo " "
			echo "     Spit (Sp) : splits the screen the usual way, but allows"
			echo "       multiple files splitting."
			echo "     Usage: sp a b c..."
			echo "       It is allowed to match multiple files with the *"
			echo "       (regexp) character."
			echo "       (sp is remapped to this function.)"
			echo " "
		else
			let i = 0
			while i < len( l:myargs )
				execute 'let file = expand( l:myargs[ ' . i .' ] )'
				if match( file, '/\*/' )
					let l:files = expand( file )
					while l:files != ""
						let l:thisfile = substitute( l:files, "\n.*$", "", "" )
						let l:files = substitute( l:files, l:thisfile, "", "" )
						let l:files = substitute( l:files, "^\n", "", "" )
						execute l:sp . ' ' . l:thisfile
					endwhile
				else
					execute l:sp . ' ' . file
				endif
				let i = i + 1
			endwhile
		end
	endif
endfunction
com! -nargs=* -complete=file Spit call Spit(<f-args>)
com! -nargs=* -complete=file Sp call Spit(<f-args>)
cab sp Spit

function! Vspit(...)
	let l:sp = 'vsplit'
	if a:0 == 0
		execute l:sp
	else
		" Put all the arguments in a list, in case it has got a list already
		if a:0 == 1 && type( a:1 ) == 3
			" Type == 3 is a list
			let l:myargs = a:1
		else
			let l:myargs = a:000
		endif
		"if a:1 == "--help"
		if l:myargs[ 0 ] == "--help"
			echo " "
			echo "     Vspit (Vsp) : splits vertically the screen the usual way, but allows"
			echo "       multiple files splitting."
			echo "     Usage: vsp a b c..."
			echo "       It is allowed to match multiple files with the *"
			echo "       (regexp) character."
			echo "       (vsp is remapped to this function.)"
			echo " "
		else
			let i = 0
			while i < len( l:myargs )
				execute 'let file = expand( l:myargs[ ' . i .' ] )'
				if match( file, '/\*/' )
					let l:files = expand( file )
					while l:files != ""
						let l:thisfile = substitute( l:files, "\n.*$", "", "" )
						let l:files = substitute( l:files, l:thisfile, "", "" )
						let l:files = substitute( l:files, "^\n", "", "" )
						execute l:sp . ' ' . l:thisfile
					endwhile
				else
					execute l:sp . ' ' . file
				endif
				let i = i + 1
			endwhile
		end
	endif
endfunction
com! -nargs=* -complete=file Vspit call Vspit(<f-args>)
com! -nargs=* -complete=file Vsp call Vspit(<f-args>)
cab vsp Vspit

function! SpitUp(...)
	let l:isbelow = &splitbelow
	if l:isbelow == 1
		set invsplitbelow
	endif
	call Spit( a:000 )
	if l:isbelow == 1
		set invsplitbelow
	endif
endfunction
com! -nargs=* -complete=file SpitUp call SpitUp(<f-args>)
com! -nargs=* -complete=file Spu call SpitUp(<f-args>)
cab spu SpitUp

function! SpitDown(...)
	let l:isbelow = &splitbelow
	if l:isbelow == 0
		set invsplitbelow
	endif
	call Spit( a:000 )
	if l:isbelow == 0
		set invsplitbelow
	endif
endfunction
com! -nargs=* -complete=file SpitDown call SpitDown(<f-args>)
com! -nargs=* -complete=file Spd call SpitDown(<f-args>)
cab spd SpitDown

function! VspitRight(...)
	let l:isright = &splitright
	if l:isright == 0
		set invsplitright
	endif
	call Vspit( a:000 )
	if l:isright == 0
		set invsplitright
	endif
endfunction
com! -nargs=* -complete=file VspitRight call VspitRight(<f-args>)
com! -nargs=* -complete=file Vspr call VspitRight(<f-args>)
cab vspr VspitRight

function! VspitLeft(...)
	let l:isright = &splitright
	if l:isright == 1
		set invsplitright
	endif
	call Vspit( a:000 )
	if l:isright == 1
		set invsplitright
	endif
endfunction
com! -nargs=* -complete=file VspitLeft call VspitLeft(<f-args>)
com! -nargs=* -complete=file Vspl call VspitLeft(<f-args>)
cab vspl VspitLeft

