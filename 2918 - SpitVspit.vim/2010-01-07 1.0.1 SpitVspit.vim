"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""" This is vimsplit function to split among many files even :sp *.cpp<cr> works!! """"""""""
"""""""""""""""""""""""""""" Function written by Gael Induni """"""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""" Version 1.0 """"""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""" On January 5, 2010 """""""""""""""""""""""""""""""""""""""""""""""""
""" Inspired from http://vim.wikia.com/wiki/Opening_multiple_files_from_a_single_command-line """
"""""""""""""""" By salmanhalim """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! Spit(...)
	let l:sp = 'split'
	if a:0 == 0
		execute l:sp
	else
		if a:1 == "--help"
			echo " "
			echo "     Spit (Sp) : splits the screen the usual way, but allows"
			echo "       multiple files splitting."
			echo "     Usage: sp a b c..."
			echo "       It is allowed to match multiple files with the *"
			echo "       (regexp) character."
			echo "       (sp is remapped to this function.)"
			echo " "
		else
			let i = 1
			while i <= a:0
				execute 'let file = expand(a:' . i .')'
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
		if a:1 == "--help"
			echo " "
			echo "     Vspit (Vsp) : splits vertically the screen the usual way, but allows"
			echo "       multiple files splitting."
			echo "     Usage: vsp a b c..."
			echo "       It is allowed to match multiple files with the *"
			echo "       (regexp) character."
			echo "       (vsp is remapped to this function.)"
			echo " "
		else
			let i = 1
			while i <= a:0
				execute 'let file = expand(a:' . i .')'
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

