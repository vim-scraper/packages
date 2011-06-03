"=============================================================================
" File:        R.vim
" Author:      Vincent Nijs (vincent.nijs@gmail.com)
" Last Change: Mon Dec  3 15:28:23 CST 2007
" Version:     0.01
"=============================================================================

" map <F3> to run the R script, vim will wait for the output
map <buffer> <F3>	:call RunRProgram("selectedlines")<CR><CR>
imap <buffer> <F3>	<ESC>:call RunRProgram("selectedlines")<CR><CR>a

" map <F4> to run the R script, vim will wait for the output
map <buffer> <F4>	:call RunRProgram("quick")<CR><CR>
imap <buffer> <F4>	<ESC>:call RunRProgram("quick")<CR><CR>a

" map <F5> to run the R script, long mode. You will see a tail of the output file
map <buffer> <F5>	:call RunRProgram("long")<CR><CR>
imap <buffer> <F5>	<ESC>:call RunRProgram("long")<CR><CR>a

" map <F6> to run the Ruby script, vim will wait for the output
map <buffer> <F6>	:call OpenROutput()<CR>
imap <buffer> <F6>	<ESC>:call OpenROutput()<CR>a

" comment lines in/out
noremap <buffer> mc	:call RToggleCommentify()<CR>j^

" highlighting the braces
syn match			RBraces	"[\{\}\[\]\(\)]"
hi RBraces		ctermfg=9 guifg=orange

" -----------------------------------------------------------------------------
"  R functions
" -----------------------------------------------------------------------------
function! RunRProgram(mode) range " {{{
	" write buffer
	execute 'w!'

	" get the name of the current buffer, plus the full path
	let codeBuffer = expand("%:p")

	" set the name for the output buffer
	let outputBuffer = expand("%:r:h").'.rout'

	if a:mode == "selectedlines"
		" sending selected lines to interactive R application
		let command = join(getline(a:firstline,a:lastline),"\\n")
		let command = substitute(command,"\"","\\\\\"","g")
		let command = substitute(command,"\'","\\\\\"","g")
		call system("osascript -e 'tell application \"R\" to cmd \"" .command. "\"'")
		
	elseif a:mode == "quick"
		" opening a buffer for the output
		execute 'e! ' outputBuffer

		" clearing contents of the output buffer
		execute '%d'

		" Only one window visible
		execute 'only'

		" run the R program
		execute '!R --save < ' codeBuffer ' > ' outputBuffer ' 2>&1'
		"call system('R --save < ' codeBuffer ' > ' outputBuffer ' 2>&1')


		" open the changed file
		execute 'e! ' outputBuffer		
		execute 'normal gg'

		" if there are errors in the output file jump to the first one
		if search(".rb:[0-9]\*:","W") == 0
			execute 'normal G' 
		endif

	elseif a:mode == "long"

		let batchFileBuffer = "~/myVimFiles/vimSwap/rVim.bat"
		execute 'e! ' batchFileBuffer	
		" clearing contents of buffer
		execute '%d'
		" creating a command file that can be run externally using the 'tee' command
		call append(0, 'R --save < ' .codeBuffer. ' > ' .outputBuffer. ' 2>&1')

		execute 'w ' batchFileBuffer	
		execute '!chmod +x  ' batchFileBuffer
		execute '!nohup ' batchFileBuffer ' > ~/myVimFiles/vimSwap/nohup.out &'

		" delete the temporary buffer after the program has been started
		execute 'bd! rVim.bat' 

		" keeping track of the output
		execute '!tail -f ' outputBuffer	

	endif

endfunction "}}}
function! OpenROutput() "{{{

	" set the name for the output buffer
	let outputBuffer = expand("%:r:h").'.rout'

	" opening the output buffer
	execute 'e! ' outputBuffer

	" Only one window visible
	execute 'only'

endfunction "}}}
function! RToggleCommentify() "{{{
	let lineString = getline(".")
	if lineString != $									" don't comment empty lines
		let commentSymbol = '###'

		let isCommented = stridx(lineString,commentSymbol)		" getting the first 3 symbols
		if isCommented == -1					
			call Commentify(commentSymbol)				" if the line is uncommented, comment
		else
			call UnCommentify(commentSymbol)			" if the line is already commented, uncomment
		endif
	endif
endfunction "}}}
function! Commentify(commentSymbol) "{{{
	execute 'normal ^i'.a:commentSymbol.' '
endfunction "}}}
function! UnCommentify(commentSymbol) "{{{
	set nohlsearch	
	execute ':s+'.a:commentSymbol.'\s*++'
	set hlsearch	
endfunction "}}}

